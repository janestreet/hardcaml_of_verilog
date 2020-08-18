open! Import

type t = Synthesized_design.t Map.M(String).t [@@deriving sexp_of]

let length t = Map.length t

exception Cell_not_in_techlib of string * string
exception Failed_to_find_net of int
exception Input_not_found of string
exception Empty_bus of string

type 'json_netlist synthesized_design_converter =
  ?allow_blackboxes:bool
  -> ?use_netlist_names:bool
  -> techlib:Techlib.t
  -> 'json_netlist
  -> t Or_error.t

type 'json_netlist synthesized_design_converter_exn =
  ?allow_blackboxes:bool
  -> ?use_netlist_names:bool
  -> techlib:Techlib.t
  -> 'json_netlist
  -> t

(* implementation of the conversion routines *)
module Conversion = struct
  module N = struct
    module T = struct
      type t = int list [@@deriving compare, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end

  module Y = Yosys_atd_t
  open Hardcaml

  let get_net_name_map netnames =
    let add map (name, netname) =
      let bits = List.map ~f:Cell_reference.net_of_bit netname.Y.bits in
      if Map.mem map bits
      then (
        let n = Map.find_exn map bits in
        Map.set map ~key:bits ~data:(name :: n))
      else Map.set map ~key:bits ~data:[ name ]
    in
    List.fold ~f:add ~init:(Map.empty (module N)) netnames
  ;;

  (* something drives a net; it's either a module input, or a cell output.

     create wires and a map from bits (nets) to wires/indexes for module
     inputs and cell outputs *)
  let modl_wire_map ~busnames ~ports ~cells =
    let map =
      List.fold
        ~f:(fun map (b, s) -> Map.set map ~key:b ~data:(0, s))
        ~init:(Map.empty (module Int))
        [ 0, Signal.gnd; 1, Signal.vdd ]
    in
    (* generate a wire, and create a mapping from the net index to the wire and offset *)
    let wire_with_name busnames bits_ =
      let open Signal in
      let wire = wire (List.length bits_) in
      match Map.find busnames bits_ with
      | Some names -> wire, names
      | None -> wire, []
    in
    let map_of_bits map (name, bits_) =
      let wire, bname = wire_with_name busnames bits_ in
      ( (name, wire, bname)
      , snd
        @@ List.fold
             ~f:(fun (idx, map) bit ->
               (* note; adding [bit] if it is already in the list
                  should be an error (well...except for trisates!) *)
               (*printf "%i -> %Li[%i]\n" bit (Signal.Types.uid wire) idx;*)
               idx + 1, Map.set map ~key:bit ~data:(idx, wire))
             ~init:(0, map)
             bits_ )
    in
    let map_with_list f map x =
      let ws, map =
        List.fold
          ~f:(fun (ws, map) x ->
            let w, map = f map x in
            w :: ws, map)
          ~init:([], map)
          x
      in
      List.rev ws, map
    in
    let map_with_smap f map x =
      let ws, map = map_with_list f map x in
      ( List.fold_right
          ~f:(fun (n, s, m) map -> Map.set map ~key:n ~data:(s, m))
          ws
          ~init:(Map.empty (module String))
      , map )
    in
    (* mapping to module inputs *)
    let mi, map =
      map_with_smap
        (fun map (n, (port : Y.port)) ->
           map_of_bits map (n, List.map ~f:Cell_reference.net_of_bit port.Y.bits))
        map
      @@ List.filter ~f:(fun (_, port) -> Poly.equal port.Y.direction `Input) ports
    in
    (* mapping to cell outputs *)
    let co, map =
      map_with_list
        (fun map (_, x, _) -> map_with_smap map_of_bits map x.Cell_reference.outputs)
        map
        cells
    in
    mi, co, map
  ;;

  (* create black box for cell *)
  let black_box_of_cell inst_name cell =
    let _, outputs = Cell_reference.partition_ios cell in
    let f _ p i =
      let inst =
        Instantiation.create
          ()
          ~instance:inst_name
          ~name:cell.Y.typ
          ~parameters:p
          ~inputs:i
          ~outputs:(List.map ~f:(fun (n, b) -> n, List.length b) outputs)
      in
      List.map ~f:(fun (n, _) -> n, Map.find_exn inst n) outputs
    in
    f
  ;;

  let get_bus cell_name map bus =
    let open Signal in
    let find b =
      match Map.find map b with
      | Some b -> b
      | None -> raise (Failed_to_find_net b)
    in
    let simple = false in
    if simple
    then
      concat_lsb
      @@ List.map
           ~f:(fun b ->
             let i, w = find b in
             bit w i)
           bus
    else (
      (* consolidate bus, where possible *)
      let rec opt (w, l, h) bus =
        match bus with
        | [] -> [ w, l, h ]
        | b :: bus ->
          let open Signal in
          let i, w' = find b in
          if i = h + 1 && Uid.equal (uid w') (uid w)
          then opt (w, l, h + 1) bus
          else (w, l, h) :: opt (w', i, i) bus
      in
      match bus with
      | [] -> raise (Empty_bus cell_name)
      | h :: t ->
        let i, w = find h in
        let l = opt (w, i, i) t in
        concat_lsb @@ List.map ~f:(fun (w, l, h) -> select w h l) l)
  ;;

  let get_bus_of_nets name map bus =
    get_bus name map (List.map ~f:Cell_reference.net_of_bit bus)
  ;;

  let load_modl
        ~blackbox
        ~keepnames
        { Techlib.blackboxes; cell_fns = techlib }
        (name, modl)
    =
    let bbmap =
      List.fold
        ~f:(fun map m -> Map.set map ~key:("$" ^ m) ~data:m)
        ~init:(Map.empty (module String))
        blackboxes
    in
    (* find cell in the techlib *)
    let find_cell inst_name cell =
      (* match the cell in the techlib *)
      match List.Assoc.find techlib cell.Y.typ ~equal:String.equal with
      | Some cell -> cell
      | None ->
        ((* could be a blackbox cell from the techlib *)
          match Map.find bbmap cell.Y.typ with
          | Some n -> black_box_of_cell inst_name { cell with Y.typ = n }
          (* otherwise pure blackbox, if allowed *)
          | None when blackbox -> black_box_of_cell inst_name cell
          | None -> raise Y.(Cell_not_in_techlib (cell.typ, cell.attributes.src)))
    in
    (* get cell with explicit inputs and outputs and map to techlib *)
    let create_cell (inst_name, cell) =
      let cell' = Cell_reference.create_cell cell in
      inst_name, cell', (find_cell inst_name cell) cell'
    in
    (* instantiate all the cells *)
    let open Signal in
    let instantiate_cell map (cell_name, cell, cell_fn) co =
      (* create parameters *)
      let params =
        Cell_reference.create_params cell_name cell.Cell_reference.parameters
      in
      (* create input busses *)
      let inputs =
        List.map
          ~f:(fun (name, bits) -> name, get_bus (cell_name ^ ": " ^ name) map bits)
          cell.Cell_reference.inputs
      in
      (* instantiate module *)
      let outputs = cell_fn params inputs in
      (* connect outputs *)
      let ( -- ) a b = List.fold ~f:( -- ) ~init:a b in
      List.iter
        ~f:(fun (n, o) ->
          let w, m = Map.find_exn co n in
          w <== o -- m)
        outputs
    in
    let instantiate map cells co = List.iter2_exn ~f:(instantiate_cell map) cells co in
    (* connect together wires *)
    let connect_inputs inputs mi =
      let find n =
        match Map.find mi n with
        | Some (a, _) -> a
        | None -> raise (Input_not_found n)
      in
      List.iter ~f:(fun (n, i) -> find n <== i) inputs
    in
    let collect_outputs map outputs =
      List.map
        ~f:(fun (n, (port : Y.port)) ->
          n, get_bus_of_nets ("output: " ^ n) map port.Y.bits)
        outputs
    in
    let load_modl modl =
      let cells = List.map ~f:create_cell modl.Y.cells in
      let busnames =
        if keepnames then get_net_name_map modl.Y.netnames else Map.empty (module N)
      in
      (*let cells = convert_memories cells in*)
      let mi, co, map = modl_wire_map ~busnames ~ports:modl.Y.ports ~cells in
      instantiate map cells co;
      mi, co, map
    in
    let is_input (_, (p : Y.port)) = Poly.equal p.Y.direction `Input in
    let is_output (_, (p : Y.port)) = Poly.equal p.Y.direction `Output in
    let inputs, outputs = List.partition_tf ~f:is_input modl.Y.ports in
    let port (n, (p : Y.port)) = n, List.length p.Y.bits in
    let inputs, outputs = List.map ~f:port inputs, List.map ~f:port outputs in
    let create_fn i =
      let mi, _, map = load_modl modl in
      let outputs = List.filter ~f:is_output modl.Y.ports in
      connect_inputs i mi;
      collect_outputs map outputs
    in
    name, Synthesized_design.create ~name ~inputs ~outputs create_fn
  ;;

  let load ?(allow_blackboxes = true) ?(use_netlist_names = true) ~techlib t =
    let designs =
      List.map
        ~f:(load_modl ~blackbox:allow_blackboxes ~keepnames:use_netlist_names techlib)
        t.Y.modl
    in
    Map.of_alist_exn (module String) designs
  ;;
end

(* exported API *)

let of_json_netlist_exn ?allow_blackboxes ?use_netlist_names ~techlib json_netlist =
  Conversion.load ?allow_blackboxes ?use_netlist_names ~techlib json_netlist
;;

let of_json_netlist ?allow_blackboxes ?use_netlist_names ~techlib json_netlist =
  Or_error.try_with (fun () ->
    of_json_netlist_exn ?allow_blackboxes ?use_netlist_names ~techlib json_netlist)
;;
