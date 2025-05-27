open Base
module Bus = Netlist.Bus
module Cell = Netlist.Cell
module Port = Netlist.Port

type t =
  { module_name : string
  ; inputs : int Port.t list
  ; outputs : int Port.t list
  ; create_fn :
      (Hardcaml.Signal.t Port.t list -> Hardcaml.Signal.t Port.t list Or_error.t
      [@sexp.opaque])
  }
[@@deriving sexp_of, fields ~getters]

let cell_outputs bus_map (cell : Cell.t) =
  List.filter_map cell.outputs ~f:(fun port ->
    Circuit_bus_map.wire_of_cell_output bus_map cell port)
  |> Or_error.all
;;

let cell_inputs bus_map (cell : Cell.t) =
  List.filter_map cell.inputs ~f:(fun port ->
    Circuit_bus_map.signal_of_bus_if_not_empty bus_map port)
  |> Or_error.all
;;

let instantiate_cell_not_in_techlib (bus_map : Circuit_bus_map.t) (cell : Cell.t) =
  (* Create the inputs and outputs of each cell from the bus map. Filter completely empty
     ports. *)
  let%bind.Or_error inputs = cell_inputs bus_map cell in
  let%bind.Or_error outputs = cell_outputs bus_map cell in
  let inst =
    Hardcaml.Instantiation.create
      ~name:cell.module_name
      ~instance:cell.instance_name
      ~parameters:cell.parameters
      ~inputs:(List.map inputs ~f:(fun { Port.name; value } -> name, value))
      ~outputs:
        (List.map outputs ~f:(fun { Port.name; value } ->
           name, Hardcaml.Signal.width value))
      ()
  in
  (* Attach cell outputs to their wires *)
  List.iter outputs ~f:(fun { name; value = signal } ->
    Hardcaml.Signal.( <-- ) signal (Hardcaml.Instantiation.output inst name));
  Ok ()
;;

let assign_cell_outputs
  cell
  (cell_outputs : Hardcaml.Signal.t Port.t list)
  (implementation_outputs : Hardcaml.Signal.t Port.t list)
  =
  let find name =
    match List.find cell_outputs ~f:(fun cell -> String.equal cell.name name) with
    | None ->
      Or_error.error_s
        [%message
          "Failed to associate cell output port with signal"
            (name : string)
            (cell : Cell.t)]
    | Some s -> Ok s
  in
  let%bind.Or_error () =
    List.map implementation_outputs ~f:(fun implementation_output ->
      let%bind.Or_error cell_output = find implementation_output.name in
      Hardcaml.Signal.( <-- ) cell_output.value implementation_output.value;
      Ok ())
    |> Or_error.all_unit
  in
  Ok ()
;;

let instantiate_cell bus_map (cell : Cell.t) =
  match Techlib.find Techlib.cells cell.module_name with
  | None -> instantiate_cell_not_in_techlib bus_map cell
  | Some (cell_implementation : Techlib.Cell_implementation.create_fn) ->
    let%bind.Or_error inputs = cell_inputs bus_map cell in
    let%bind.Or_error cell_outputs = cell_outputs bus_map cell in
    let%bind.Or_error implementation_outputs =
      cell_implementation cell cell.parameters inputs
    in
    let%bind.Or_error () = assign_cell_outputs cell cell_outputs implementation_outputs in
    Ok ()
;;

let instantiate_cells bus_map cells =
  List.map cells ~f:(instantiate_cell bus_map) |> Or_error.all_unit
;;

let create_circuit_outputs bus_map (outputs : Bus.t Port.t list) =
  List.map outputs ~f:(fun port ->
    match Circuit_bus_map.signal_of_bus_if_not_empty bus_map port with
    | Some port -> port
    | None ->
      Or_error.error_s
        [%message "Cannot construct bus for top level output port" (port : Bus.t Port.t)])
  |> Or_error.all
;;

let create netlist ~top_name =
  let%bind.Or_error top = Netlist.find_module_by_name netlist top_name in
  let create_fn circuit_inputs =
    (* Add circuit inputs and cell outputs to a map of signal drivers. *)
    let%bind.Or_error bus_map = Circuit_bus_map.create top ~circuit_inputs in
    (* Create instantiations for all cells, connecting inputs and outputs *)
    let%bind.Or_error () = instantiate_cells bus_map top.cells in
    (* Construct circuit outputs *)
    let%bind.Or_error outputs = create_circuit_outputs bus_map top.outputs in
    Ok outputs
  in
  let to_port (p : _ Port.t) = { p with value = List.length p.value } in
  Ok
    { module_name = top.name
    ; inputs = List.map top.inputs ~f:to_port
    ; outputs = List.map top.outputs ~f:to_port
    ; create_fn
    }
;;

let to_hardcaml_circuit t =
  let create_fn = create_fn t in
  let inputs =
    List.map (inputs t) ~f:(fun { Port.name; value = width } ->
      { Port.name; value = Hardcaml.Signal.input name width })
  in
  let%bind.Or_error outputs = create_fn inputs in
  let outputs =
    List.map outputs ~f:(fun { Port.name; value } -> Hardcaml.Signal.output name value)
  in
  try Ok (Hardcaml.Circuit.create_exn ~name:t.module_name outputs) with
  | e ->
    Or_error.error_s
      [%message "Failed to convert verilog design to a hardcaml circuit" (e : exn)]
;;
