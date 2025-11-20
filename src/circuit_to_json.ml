open Hardcaml
open Base
open Yosys_netlist

let print_s = Stdio.Out_channel.print_s

let port_name signal =
  match Signal.names signal with
  | [ name ] -> name
  | _ -> raise_s [%message "Invalid circuit port name"]
;;

let signal_op_to_string op =
  match (op : Signal.Type.Op.t) with
  | Add -> "$add"
  | Sub -> "$sub"
  | Mulu -> "$mulu"
  | Muls -> "$muls"
  | And -> "$and"
  | Or -> "$or"
  | Xor -> "$xor"
  | Eq -> "$eq"
  | Lt -> "$lt"
;;

let create_module ~debug circuit =
  (* Create a set of signals we aren't rendering, so we should ignore them. *)
  let ignore_set = ref (Set.empty (module Signal.Type.Uid)) in
  Signal_graph.iter (Circuit.signal_graph circuit) ~f:(fun signal ->
    match signal with
    | Reg { register = { reset; clear; _ }; _ } ->
      Option.iter clear ~f:(fun { clear_to; _ } ->
        ignore_set := Set.add !ignore_set (Signal.uid clear_to));
      Option.iter reset ~f:(fun { reset_to; _ } ->
        ignore_set := Set.add !ignore_set (Signal.uid reset_to))
    | _ -> ());
  (* Create a map of signal uids which will be outputs of instances, with a list of
     selects driven by that uid. This will be used to correctly assign signals to outputs
     of instances. *)
  let select_map = ref (Map.empty (module Signal.Type.Uid)) in
  Signal_graph.iter (Circuit.signal_graph circuit) ~f:(fun signal ->
    match signal with
    | Inst { info; _ } -> select_map := Map.set !select_map ~key:info.uid ~data:[]
    | _ -> ());
  Signal_graph.iter (Circuit.signal_graph circuit) ~f:(fun signal ->
    match signal with
    | Select { arg; info; high; low } ->
      (* Only add it if it is driven by an Inst output. *)
      (match Map.find !select_map (Signal.uid arg) with
       | Some v ->
         select_map
         := Map.set !select_map ~key:(Signal.uid arg) ~data:((info.uid, high, low) :: v)
       | None -> ())
    | _ -> ());
  (* We create a map of [Info.t]s that when seen we want to replace the [Info], this is
     used when dealing with wires. *)
  let driver_map = ref (Map.empty (module Signal.Type.Uid)) in
  Signal_graph.iter (Circuit.signal_graph circuit) ~f:(fun signal ->
    match signal with
    | Wire { info; driver = Some driver } ->
      (match Map.add !driver_map ~key:info.uid ~data:(Signal.uid driver) with
       | `Ok new_map -> driver_map := new_map
       | _ -> ())
    | _ -> ());
  if debug
  then (
    print_s [%message (!ignore_set : Set.M(Signal.Type.Uid).t)];
    print_s [%message (!driver_map : Signal.Type.Uid.t Map.M(Signal.Type.Uid).t)];
    print_s
      [%message
        (!select_map : (Signal.Type.Uid.t * int * int) list Map.M(Signal.Type.Uid).t)]);
  let rec get_driver uid =
    match Map.find !driver_map uid with
    | Some v -> get_driver v
    | None -> uid
  in
  let bit_name_of_uid uid = Bit.Index (uid |> get_driver |> Signal.Type.Uid.to_int) in
  let bit_name_of_signal signal =
    Bit.Index (Signal.uid signal |> get_driver |> Signal.Type.Uid.to_int)
  in
  let bit_name_of_signal_opt = function
    | None -> bit_name_of_signal Signal.empty
    | Some signal -> bit_name_of_signal signal
  in
  let create_cells circuit =
    (* let default_attributes : attributes =
     *   { src = ""; full_case = 0; parallel_case = 0; init = None; unused_bits = None }
     * in *)
    let default_cell =
      { Cell.V.module_name = ""
      ; parameters = []
      ; port_directions = []
      ; connections = []
      ; hide_name = 0
      }
    in
    let cells = ref ([] : (string * Cell.V.t) list) in
    Signal_graph.iter (Circuit.signal_graph circuit) ~f:(fun signal ->
      if debug then Stdio.printf "%s\n" (Signal.to_string signal);
      let cell =
        let connections =
          List.map ~f:(fun (name, bits) -> Connection.{ name; value = bits })
        in
        let port_dirns =
          List.map ~f:(fun (name, dirn) -> Port_direction.{ name; value = dirn })
        in
        let open Direction in
        match signal with
        | Reg { d; info; register } ->
          Some
            ( "$procdff$" ^ Signal.Type.Uid.to_string info.uid
            , { default_cell with
                module_name = "$our_dff"
              ; connections =
                  [ "D", [ bit_name_of_signal d ]
                  ; ( "CLR"
                    , [ bit_name_of_signal_opt
                          (Option.map register.clear ~f:(fun clear -> clear.clear))
                      ] )
                  ; ( "RST"
                    , [ bit_name_of_signal_opt
                          (Option.map register.reset ~f:(fun reset -> reset.reset))
                      ] )
                  ; "CLK", [ bit_name_of_signal register.clock.clock ]
                  ; "CE", [ bit_name_of_signal_opt register.enable ]
                  ; "Q", [ bit_name_of_uid info.uid ]
                  ]
                  |> connections
              ; port_directions =
                  [ "CLK", Input
                  ; "CE", Input
                  ; "CLR", Input
                  ; "RST", Input
                  ; "D", Input
                  ; "Q", Output
                  ]
                  |> port_dirns
              } )
        | Cat { info; args } ->
          Some
            ( "$mygate" ^ Signal.Type.Uid.to_string info.uid
            , { default_cell with
                module_name = "$cat"
              ; connections =
                  [ "A", List.map args ~f:bit_name_of_signal
                  ; "Y", [ bit_name_of_uid info.uid ]
                  ]
                  |> connections
              ; port_directions = [ "A", Input; "Y", Output ] |> port_dirns
              } )
        | Empty -> None
        | Const { info; constant } ->
          if Set.exists !ignore_set ~f:(Signal.Type.Uid.equal info.uid)
          then None
          else (
            let name =
              "$"
              ^ (match Bits.width constant with
                 | 1 -> if Bits.to_bool constant then "vdd" else "gnd"
                 | _ -> "const " ^ Int.Hex.to_string (Bits.to_int_trunc constant))
              ^ "_"
              ^ Signal.Type.Uid.to_string info.uid
            in
            Some
              ( name
              , { default_cell with
                  module_name = name
                ; connections = [ "Y", [ bit_name_of_uid info.uid ] ] |> connections
                ; port_directions = [ "Y", Output ] |> port_dirns
                } ))
        | Not { arg; info } ->
          Some
            ( "$not" ^ Signal.Type.Uid.to_string info.uid
            , { default_cell with
                module_name = "$inv"
              ; connections =
                  [ "A", [ bit_name_of_signal arg ]; "Y", [ bit_name_of_uid info.uid ] ]
                  |> connections
              ; port_directions = [ "A", Input; "Y", Output ] |> port_dirns
              } )
        | Wire _ -> None
        | Select { arg; info; high; low } ->
          (* Don't draw the select if it is driven by an Inst. *)
          (match Map.find !select_map (Signal.uid arg) with
           | None ->
             Some
               (let select_name =
                  "$select"
                  ^ Signal.Type.Uid.to_string info.uid
                  ^ "["
                  ^ Int.to_string high
                  ^ ":"
                  ^ Int.to_string low
                  ^ "]"
                in
                ( select_name
                , { default_cell with
                    module_name = select_name
                  ; connections =
                      [ "A", [ bit_name_of_signal arg ]
                      ; "Y", [ bit_name_of_uid info.uid ]
                      ]
                      |> connections
                  ; port_directions = [ "A", Input; "Y", Output ] |> port_dirns
                  } ))
           | _ -> None)
        | Multiport_mem { info; write_ports; _ } ->
          Some
            ( "$memory" ^ Signal.Type.Uid.to_string info.uid
            , { default_cell with
                module_name = "$multiportmem"
              ; connections =
                  List.concat
                    Array.(
                      mapi write_ports ~f:(fun i a ->
                        [ "WR_DATA" ^ Int.to_string i, [ bit_name_of_signal a.write_data ]
                        ; "WR_EN" ^ Int.to_string i, [ bit_name_of_signal a.write_enable ]
                        ; ( "WR_ADDR" ^ Int.to_string i
                          , [ bit_name_of_signal a.write_address ] )
                        ; "WR_CLK" ^ Int.to_string i, [ bit_name_of_signal a.write_clock ]
                        ])
                      |> to_list)
                  @ [ "A", [ bit_name_of_uid info.uid ] ]
                  |> connections
              ; port_directions =
                  List.concat
                    Array.(
                      mapi write_ports ~f:(fun i _ ->
                        [ "WR_DATA" ^ Int.to_string i, Input
                        ; "WR_EN" ^ Int.to_string i, Input
                        ; "WR_ADDR" ^ Int.to_string i, Input
                        ; "WR_CLK" ^ Int.to_string i, Input
                        ])
                      |> to_list)
                  @ [ "A", Input ]
                  |> port_dirns
              } )
        | Mem_read_port { info; _ } ->
          Some
            ( "$mem_read_port" ^ Signal.Type.Uid.to_string info.uid
            , { default_cell with
                module_name = "$memreadport"
              ; connections = [ "A", [ bit_name_of_uid info.uid ] ] |> connections
              ; port_directions = [ "A", Input ] |> port_dirns
              } )
        | Op2 { info; op; arg_a; arg_b } ->
          Some
            ( "$gate" ^ Signal.Type.Uid.to_string info.uid
            , { default_cell with
                module_name = signal_op_to_string op
              ; connections =
                  [ "A", [ bit_name_of_signal arg_a ]
                  ; "B", [ bit_name_of_signal arg_b ]
                  ; "Y", [ bit_name_of_uid info.uid ]
                  ]
                  |> connections
              ; port_directions = [ "A", Input; "B", Input; "Y", Output ] |> port_dirns
              } )
        | Mux { info; select; cases } ->
          Some
            ( "$mux" ^ Signal.Type.Uid.to_string info.uid
            , { default_cell with
                module_name = "$our_mux"
              ; connections =
                  List.mapi cases ~f:(fun i a ->
                    "A" ^ Int.to_string i, [ bit_name_of_signal a ])
                  @ [ "S", [ bit_name_of_signal select ] ]
                  @ [ "Y", [ bit_name_of_uid info.uid ] ]
                  |> connections
              ; port_directions =
                  List.mapi cases ~f:(fun i _ -> "A" ^ Int.to_string i, Input)
                  @ [ "S", Input ]
                  @ [ "Y", Output ]
                  |> port_dirns
              } )
        | Cases { info; select; cases; default } ->
          Some
            ( "$cases" ^ Signal.Type.Uid.to_string info.uid
            , { default_cell with
                module_name = "$our_cases"
              ; connections =
                  (List.mapi cases ~f:(fun i (match_with, value) ->
                     [ "M" ^ Int.to_string i, [ bit_name_of_signal match_with ]
                     ; "V" ^ Int.to_string i, [ bit_name_of_signal value ]
                     ])
                   |> List.concat)
                  @ [ "S", [ bit_name_of_signal select ]
                    ; "D", [ bit_name_of_signal default ]
                    ; "Y", [ bit_name_of_uid info.uid ]
                    ]
                  |> connections
              ; port_directions =
                  (List.mapi cases ~f:(fun i _ ->
                     [ "M" ^ Int.to_string i, Input; "V" ^ Int.to_string i, Input ])
                   |> List.concat)
                  @ [ "S", Input; "D", Input; "Y", Output ]
                  |> port_dirns
              } )
        | Inst { info; instantiation; _ } ->
          (* Get the list of selects this instance drives. *)
          let selects = Map.find_exn !select_map info.uid in
          Some
            ( "$mygate" ^ Signal.Type.Uid.to_string info.uid
            , { default_cell with
                module_name = "$inst_" ^ instantiation.instance_label
              ; connections =
                  List.mapi
                    instantiation.inputs
                    ~f:(fun _i { name = n; input_signal = s } ->
                      n, [ bit_name_of_signal s ])
                  @ List.filter_map
                      instantiation.outputs
                      ~f:(fun { name = n; output_width = _; output_low_index = o_lo } ->
                        (* Try match each output with a select based on its hi and lo. *)
                        match List.find selects ~f:(fun (_id, _hi, lo) -> o_lo = lo) with
                        | Some (info, _, _) -> Some (n, [ bit_name_of_uid info ])
                        | None -> None)
                  |> connections
              ; port_directions =
                  List.mapi
                    instantiation.inputs
                    ~f:(fun _i { name = n; input_signal = _ } -> n, Input)
                  @ List.filter_map
                      instantiation.outputs
                      ~f:(fun { name = n; output_width = _; output_low_index = o_lo } ->
                        match List.find selects ~f:(fun (_id, _hi, lo) -> o_lo = lo) with
                        | Some _ -> Some (n, Output)
                        | None -> None)
                  |> port_dirns
              } )
      in
      Option.iter cell ~f:(fun cell -> cells := cell :: !cells));
    !cells
  in
  let inputs =
    List.map (Circuit.inputs circuit) ~f:(fun input ->
      Port.
        { name = port_name input
        ; value = { direction = Input; bits = [ bit_name_of_signal input ] }
        })
  in
  let outputs =
    List.map (Circuit.outputs circuit) ~f:(fun output ->
      Port.
        { name = port_name output
        ; value = { direction = Output; bits = [ bit_name_of_signal output ] }
        })
  in
  { Module.name = Circuit.name circuit
  ; value =
      { ports = inputs @ outputs
      ; cells =
          List.map (create_cells circuit) ~f:(fun (name, cell) ->
            { Cell.name; value = cell })
      ; netnames = []
      }
  }
;;

let convert ?(debug = false) circuit =
  { creator = "hardcaml"; modules = [ create_module circuit ~debug ] }
;;
