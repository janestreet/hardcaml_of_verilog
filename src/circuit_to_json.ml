open Hardcaml
open! Import
open Yosys_atd_t

let port_name signal =
  match Signal.names signal with
  | [ name ] -> name
  | _ -> raise_s [%message "Invalid circuit port name"]
;;

let signal_op_to_string op =
  match (op : Signal.signal_op) with
  | Signal_add -> "$add"
  | Signal_sub -> "$sub"
  | Signal_mulu -> "$mulu"
  | Signal_muls -> "$muls"
  | Signal_and -> "$and"
  | Signal_or -> "$or"
  | Signal_xor -> "$xor"
  | Signal_eq -> "$eq"
  | Signal_lt -> "$lt"
;;

let create_module ~debug circuit =
  (* Create a set of signals we aren't rendering, so we should ignore them. *)
  let ignore_set = Core.ref (Set.empty (module Int64)) in
  Signal_graph.iter (Circuit.signal_graph circuit) ~f:(fun signal ->
    match signal with
    | Reg { register; _ } ->
      ignore_set := Set.add !ignore_set (Signal.uid register.reg_clear_value);
      ignore_set := Set.add !ignore_set (Signal.uid register.reg_reset_value)
    | _ -> ());
  (* Create a map of signal uids which will be outputs of instances, with a list of
     selects driven by that uid. This will be used to correctly assign signals to outputs
     of instances. *)
  let select_map = Core.ref (Map.empty (module Int64)) in
  Signal_graph.iter (Circuit.signal_graph circuit) ~f:(fun signal ->
    match signal with
    | Inst { signal_id; _ } ->
      select_map := Map.set !select_map ~key:signal_id.s_id ~data:[]
    | _ -> ());
  Signal_graph.iter (Circuit.signal_graph circuit) ~f:(fun signal ->
    match signal with
    | Select { arg; signal_id; high; low } ->
      (* Only add it if it is driven by an Inst output. *)
      (match Map.find !select_map (Signal.uid arg) with
       | Some v ->
         select_map
         := Map.set
              !select_map
              ~key:(Signal.uid arg)
              ~data:((signal_id.s_id, high, low) :: v)
       | None -> ())
    | _ -> ());
  (* We create a map of signal_ids that when seen we want to replace the signal_id, this
     is used when dealing with wires. *)
  let driver_map = Core.ref (Map.empty (module Int64)) in
  Signal_graph.iter (Circuit.signal_graph circuit) ~f:(fun signal ->
    match signal with
    | Wire { signal_id; driver } ->
      if Signal.is_empty !driver
      then ()
      else (
        match Map.add !driver_map ~key:signal_id.s_id ~data:(Signal.uid !driver) with
        | `Ok new_map -> driver_map := new_map
        | _ -> ())
    | _ -> ());
  if debug
  then (
    Core.print_s [%message (!ignore_set : Set.M(Int64).t)];
    Core.print_s [%message (!driver_map : int64 Map.M(Int64).t)];
    Core.print_s [%message (!select_map : (int64 * int * int) list Map.M(Int64).t)]);
  let rec get_driver s_id =
    match Map.find !driver_map s_id with
    | Some v -> get_driver v
    | None -> s_id
  in
  let bit_name_of_uid uid = `Int (uid |> get_driver |> Int64.to_int_exn) in
  let bit_name_of_signal signal =
    `Int (Signal.uid signal |> get_driver |> Int64.to_int_exn)
  in
  let create_cells circuit =
    let default_attributes : attributes =
      { src = ""; full_case = 0; parallel_case = 0; init = None; unused_bits = None }
    in
    let default_cell =
      { typ = ""
      ; connections = []
      ; port_directions = []
      ; hide_name = 0
      ; parameters = []
      ; attributes = default_attributes
      }
    in
    let cells = ref ([] : (string * cell) list) in
    Signal_graph.iter (Circuit.signal_graph circuit) ~f:(fun signal ->
      if debug then Stdio.printf "%s\n" (Signal.to_string signal);
      let cell =
        match signal with
        | Reg { d; signal_id; register } ->
          Some
            ( "$procdff$" ^ Int64.to_string signal_id.s_id
            , { default_cell with
                typ = "$our_dff"
              ; connections =
                  [ "D", [ bit_name_of_signal d ]
                  ; "CLR", [ bit_name_of_signal register.reg_clear ]
                  ; "RST", [ bit_name_of_signal register.reg_reset ]
                  ; "CLK", [ bit_name_of_signal register.reg_clock ]
                  ; "CE", [ bit_name_of_signal register.reg_enable ]
                  ; "Q", [ bit_name_of_uid signal_id.s_id ]
                  ]
              ; port_directions =
                  [ "CLK", `Input
                  ; "CE", `Input
                  ; "CLR", `Input
                  ; "RST", `Input
                  ; "D", `Input
                  ; "Q", `Output
                  ]
              } )
        | Cat { signal_id; args } ->
          Some
            ( "$mygate" ^ Int64.to_string signal_id.s_id
            , { default_cell with
                typ = "$cat"
              ; connections =
                  [ "A", List.map args ~f:bit_name_of_signal
                  ; "Y", [ bit_name_of_uid signal_id.s_id ]
                  ]
              ; port_directions = [ "A", `Input; "Y", `Output ]
              } )
        | Empty -> None
        | Const { signal_id; constant } ->
          if Set.exists !ignore_set ~f:(Int64.( = ) signal_id.s_id)
          then None
          else (
            let name =
              "$"
              ^ (if Bits.is_vdd constant
                 then "vdd"
                 else if Bits.is_gnd constant
                 then "gnd"
                 else "const " ^ Int.Hex.to_string (Bits.to_int constant))
              ^ "_"
              ^ Int64.to_string signal_id.s_id
            in
            Some
              ( name
              , { default_cell with
                  typ = name
                ; connections = [ "Y", [ bit_name_of_uid signal_id.s_id ] ]
                ; port_directions = [ "Y", `Output ]
                } ))
        | Not { arg; signal_id } ->
          Some
            ( "$not" ^ Int64.to_string signal_id.s_id
            , { default_cell with
                typ = "$inv"
              ; connections =
                  [ "A", [ bit_name_of_signal arg ]
                  ; "Y", [ bit_name_of_uid signal_id.s_id ]
                  ]
              ; port_directions = [ "A", `Input; "Y", `Output ]
              } )
        | Wire _ -> None
        | Select { arg; signal_id; high; low } ->
          (* Don't draw the select if it is driven by an Inst. *)
          (match Map.find !select_map (Signal.uid arg) with
           | None ->
             Some
               (let select_name =
                  "$select"
                  ^ Int64.to_string signal_id.s_id
                  ^ "["
                  ^ Int.to_string high
                  ^ ":"
                  ^ Int.to_string low
                  ^ "]"
                in
                ( select_name
                , { default_cell with
                    typ = select_name
                  ; connections =
                      [ "A", [ bit_name_of_signal arg ]
                      ; "Y", [ bit_name_of_uid signal_id.s_id ]
                      ]
                  ; port_directions = [ "A", `Input; "Y", `Output ]
                  } ))
           | _ -> None)
        | Mem { signal_id; _ } ->
          Some
            ( "Memory"
            , { default_cell with
                typ = "$mem"
              ; connections = [ "A", [ bit_name_of_uid signal_id.s_id ] ]
              ; port_directions = [ "A", `Input ]
              } )
        | Multiport_mem { signal_id; write_ports; _ } ->
          Some
            ( "$memory" ^ Int64.to_string signal_id.s_id
            , { default_cell with
                typ = "$multiportmem"
              ; connections =
                  List.concat
                    Array.(
                      mapi write_ports ~f:(fun i a ->
                        [ ( "WR_DATA" ^ Int.to_string i
                          , [ bit_name_of_signal a.write_data ] )
                        ; ( "WR_EN" ^ Int.to_string i
                          , [ bit_name_of_signal a.write_enable ] )
                        ; ( "WR_ADDR" ^ Int.to_string i
                          , [ bit_name_of_signal a.write_address ] )
                        ; ( "WR_CLK" ^ Int.to_string i
                          , [ bit_name_of_signal a.write_clock ] )
                        ])
                      |> to_list)
                  @ [ "A", [ bit_name_of_uid signal_id.s_id ] ]
              ; port_directions =
                  List.concat
                    Array.(
                      mapi write_ports ~f:(fun i _ ->
                        [ "WR_DATA" ^ Int.to_string i, `Input
                        ; "WR_EN" ^ Int.to_string i, `Input
                        ; "WR_ADDR" ^ Int.to_string i, `Input
                        ; "WR_CLK" ^ Int.to_string i, `Input
                        ])
                      |> to_list)
                  @ [ "A", `Input ]
              } )
        | Mem_read_port { signal_id; _ } ->
          Some
            ( "$mem_read_port" ^ Int64.to_string signal_id.s_id
            , { default_cell with
                typ = "$memreadport"
              ; connections = [ "A", [ bit_name_of_uid signal_id.s_id ] ]
              ; port_directions = [ "A", `Input ]
              } )
        | Op2 { signal_id; op; arg_a; arg_b } ->
          Some
            ( "$gate" ^ Int64.to_string signal_id.s_id
            , { default_cell with
                typ = signal_op_to_string op
              ; connections =
                  [ "A", [ bit_name_of_signal arg_a ]
                  ; "B", [ bit_name_of_signal arg_b ]
                  ; "Y", [ bit_name_of_uid signal_id.s_id ]
                  ]
              ; port_directions = [ "A", `Input; "B", `Input; "Y", `Output ]
              } )
        | Mux { signal_id; select; cases } ->
          Some
            ( "$mux" ^ Int64.to_string signal_id.s_id
            , { default_cell with
                typ = "$our_mux"
              ; connections =
                  List.mapi cases ~f:(fun i a ->
                    "A" ^ Int.to_string i, [ bit_name_of_signal a ])
                  @ [ "S", [ bit_name_of_signal select ] ]
                  @ [ "Y", [ bit_name_of_uid signal_id.s_id ] ]
              ; port_directions =
                  List.mapi cases ~f:(fun i _ -> "A" ^ Int.to_string i, `Input)
                  @ [ "S", `Input ]
                  @ [ "Y", `Output ]
              } )
        | Inst { signal_id; instantiation; _ } ->
          (* Get the list of selects this instance drives. *)
          let selects = Map.find_exn !select_map signal_id.s_id in
          Some
            ( "$mygate" ^ Int64.to_string signal_id.s_id
            , { default_cell with
                typ = "$inst_" ^ instantiation.inst_instance
              ; connections =
                  List.mapi instantiation.inst_inputs ~f:(fun _i (n, s) ->
                    n, [ bit_name_of_signal s ])
                  @ List.filter_map
                      instantiation.inst_outputs
                      ~f:(fun (n, (_width, o_lo)) ->
                        (* Try match each output with a select based on its hi and lo. *)
                        match
                          List.find selects ~f:(fun (_id, _hi, lo) -> o_lo = lo)
                        with
                        | Some (signal_id, _, _) ->
                          Some (n, [ bit_name_of_uid signal_id ])
                        | None -> None)
              ; port_directions =
                  List.mapi instantiation.inst_inputs ~f:(fun _i (n, _s) -> n, `Input)
                  @ List.filter_map
                      instantiation.inst_outputs
                      ~f:(fun (n, (_width, o_lo)) ->
                        match
                          List.find selects ~f:(fun (_id, _hi, lo) -> o_lo = lo)
                        with
                        | Some _ -> Some (n, `Output)
                        | None -> None)
              } )
      in
      Option.iter cell ~f:(fun cell -> cells := cell :: !cells));
    !cells
  in
  let inputs =
    List.map (Circuit.inputs circuit) ~f:(fun input ->
      port_name input, { direction = `Input; bits = [ bit_name_of_signal input ] })
  in
  let outputs =
    List.map (Circuit.outputs circuit) ~f:(fun output ->
      port_name output, { direction = `Output; bits = [ bit_name_of_signal output ] })
  in
  ( Circuit.name circuit
  , { ports = inputs @ outputs; cells = create_cells circuit; netnames = [] } )
;;

let convert ?(debug = false) circuit =
  { creator = "hardcaml"; modl = [ create_module circuit ~debug ] }
;;

let to_string t = Yosys_atd_j.string_of_t t
