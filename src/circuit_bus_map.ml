open Base
module Bit = Netlist.Bit
module Bus = Netlist.Bus
module Bus_names = Netlist.Bus_names
module Cell = Netlist.Cell
module Port = Netlist.Port

(* Operations we require from this data structure

   1. add module inputs - signals are provided by the environment
   2. add cell outputs - signal wires are created
   3. construct a signal from above - for module outputs and cell inputs
   4. get wire for cell output so it can be assign on instantiation
   5. perform bus naming
*)

(* A [Select.t] represents a range of bits, taken from a [wire]. A bit vector is
   represented as a list of [Select.t]s (with the MSB at head of list). A bit can be
   concatented at the msb of the select list, and will be merged if possible.

   This allows us to recover busses from the netlist. *)
module Select = struct
  type t =
    { signal : Hardcaml.Signal.t
    ; high : int
    ; low : int
    }
  [@@deriving sexp_of]

  (* [t] is sorted with the msb at the head of the list. *)
  let concat_top_bit t ~top =
    match t with
    | [] -> [ top ]
    | h :: t ->
      (* If the next bit is the next consequetive bit from the same signal, then merge
         into a larger select *)
      if Hardcaml.Signal.Type.Uid.equal
           (Hardcaml.Signal.uid h.signal)
           (Hardcaml.Signal.uid top.signal)
         && top.low = h.high + 1
      then { h with high = top.low } :: t
      else top :: h :: t
  ;;

  let vdd = { signal = Hardcaml.Signal.vdd; high = 0; low = 0 }
  let gnd = { signal = Hardcaml.Signal.gnd; high = 0; low = 0 }

  let to_signal selects =
    try
      let selects =
        List.map selects ~f:(fun select ->
          select.signal.Hardcaml.Signal.:[select.high, select.low])
      in
      Ok (Hardcaml.Signal.concat_msb selects)
    with
    | e ->
      Or_error.error_s
        [%message "Unable to form signal from selects" (selects : t list) (e : exn)]
  ;;
end

module Module_input = struct
  type t =
    { input_signal : Hardcaml.Signal.t
    ; bus : Bus.t
    }
  [@@deriving sexp_of]
end

(* Associate circuit inputs (from the environment) with netlist module inputs. *)
module Module_inputs = struct
  type t = Module_input.t Port.t list

  let circuit_input_map (circuit_inputs : Hardcaml.Signal.t Port.t list) =
    List.fold
      circuit_inputs
      ~init:(Ok (Map.empty (module String)))
      ~f:(fun map input ->
        let%bind.Or_error map in
        match Map.add map ~key:input.name ~data:input.value with
        | `Ok map -> Ok map
        | `Duplicate ->
          Or_error.error_s
            [%message
              "Duplicate circuit inputs provided"
                (circuit_inputs : Hardcaml.Signal.t Port.t list)])
  ;;

  let module_input_ports
    (circuit_input_map : Hardcaml.Signal.t Map.M(String).t)
    (module_ports : Bus.t Port.t list)
    =
    List.map module_ports ~f:(fun port ->
      match Map.find circuit_input_map port.name with
      | Some input_signal ->
        Ok { port with value = { Module_input.input_signal; bus = port.value } }
      | None ->
        Or_error.error_s
          [%message
            "Unable to associate module port with supplied inputs"
              (port : Bus.t Port.t)
              (circuit_input_map : Hardcaml.Signal.t Map.M(String).t)])
    |> Or_error.all
  ;;

  let create
    (circuit_inputs : Hardcaml.Signal.t Port.t list)
    (module_ports : Bus.t Port.t list)
    : t Or_error.t
    =
    let%bind.Or_error circuit_input_map = circuit_input_map circuit_inputs in
    module_input_ports circuit_input_map module_ports
  ;;
end

(* Key to map from a cell output port to it's signal *)
module Cell_port = struct
  module T = struct
    type t =
      { cell_instance_name : string
      ; port_name : string
      }
    [@@deriving sexp_of, compare ~localize]
  end

  include T
  include Comparator.Make (T)
end

type t =
  { bit_to_select : Select.t Map.M(Int).t
      (* Map net indices to a single bit [Select.t] of the corresponding signal (input or
         cell output). *)
  ; cell_port_to_wire : Hardcaml.Signal.t Map.M(Cell_port).t
      (* Cell outputs indexed by port name and cell instantiation name *)
  ; bus_names : Bus_names.t (* Netlist bus names for recoving signal naming. *)
  }

let add_bus_to_select_map map signal (port : Bus.t Port.t) =
  let rec f bit_index map (bits : Bus.t) =
    match bits with
    | [] -> Ok map
    | bit :: bits ->
      (match bit with
       | Vdd | Gnd | X ->
         Or_error.error_s
           [%message
             "Gnd, Vdd and X are not valid driver signals"
               (port : Bus.t Port.t)
               (signal : Hardcaml.Signal.t)]
       | Index index ->
         (match
            Map.add
              map
              ~key:index
              ~data:Select.{ signal; high = bit_index; low = bit_index }
          with
          | `Ok map -> f (bit_index + 1) map bits
          | `Duplicate ->
            Or_error.error_s [%message "Driver bit already defined" (index : int)]))
  in
  f 0 map port.value
;;

let apply_names t signal bus =
  if List.is_empty (Hardcaml.Signal.names signal)
  then (
    let names = Bus_names.find t.bus_names bus in
    List.fold names ~init:signal ~f:Hardcaml.Signal.( -- ))
  else signal
;;

let add_module_input t input_signal (port : Bus.t Port.t) =
  let input_signal = apply_names t (Hardcaml.Signal.wireof input_signal) port.value in
  let%bind.Or_error bit_to_select =
    add_bus_to_select_map t.bit_to_select input_signal port
  in
  Ok { t with bit_to_select }
;;

let add_cell_port map signal (cell : Cell.t) (port : _ Port.t) =
  match
    Map.add
      map
      ~key:Cell_port.{ cell_instance_name = cell.instance_name; port_name = port.name }
      ~data:signal
  with
  | `Ok map -> Ok map
  | `Duplicate ->
    Or_error.error_s
      [%message "Cell output port is duplicated" (port : Bus.t Port.t) (cell : Cell.t)]
;;

let add_cell_output t (cell : Cell.t) (port : Bus.t Port.t) =
  match port.value with
  | [] -> Ok t
  | _ ->
    let cell_output_signal =
      apply_names t (Hardcaml.Signal.wire (List.length port.value)) port.value
    in
    let%bind.Or_error bit_to_select =
      add_bus_to_select_map t.bit_to_select cell_output_signal port
    in
    let%bind.Or_error cell_port_to_wire =
      add_cell_port t.cell_port_to_wire cell_output_signal cell port
    in
    Ok { t with bit_to_select; cell_port_to_wire }
;;

let find_and_concat_bus_bit map selects (bit : Bit.t) =
  let%bind.Or_error selects in
  match bit with
  | Vdd -> Ok (Select.vdd :: selects)
  | Gnd -> Ok (Select.gnd :: selects)
  | X -> Ok (Select.gnd :: selects)
  | Index i ->
    (match Map.find map i with
     | None -> Or_error.error_s [%message "Failed to find net in bus map" (i : int)]
     | Some select -> Ok (Select.concat_top_bit selects ~top:select))
;;

let signal_of_bus t (bus : Bus.t Port.t) =
  let%bind.Or_error selects =
    List.fold bus.value ~init:(Ok []) ~f:(fun bus ->
      find_and_concat_bus_bit t.bit_to_select bus)
  in
  let%bind.Or_error signal = Select.to_signal selects in
  Ok { Port.name = bus.name; value = apply_names t signal bus.value }
;;

let signal_of_bus_if_not_empty t (bus : Bus.t Port.t) =
  if List.is_empty bus.value then None else Some (signal_of_bus t bus)
;;

let wire_of_cell_output t (cell : Cell.t) (port : Bus.t Port.t) =
  match port.value with
  | [] -> None
  | _ ->
    (match
       Map.find
         t.cell_port_to_wire
         Cell_port.{ cell_instance_name = cell.instance_name; port_name = port.name }
     with
     | Some signal -> Some (Ok { Port.name = port.name; value = signal })
     | None ->
       Some
         (Or_error.error_s
            [%message
              "failed to find cell output port" (port : Bus.t Port.t) (cell : Cell.t)]))
;;

let empty bus_names =
  { bit_to_select = Map.empty (module Int)
  ; cell_port_to_wire = Map.empty (module Cell_port)
  ; bus_names
  }
;;

let create (module_ : Netlist.Module.t) ~circuit_inputs =
  let%bind.Or_error inputs = Module_inputs.create circuit_inputs module_.inputs in
  let bus_map = empty module_.bus_names in
  match
    List.fold inputs ~init:(Ok bus_map) ~f:(fun map port ->
      let%bind.Or_error map in
      add_module_input map port.value.input_signal { port with value = port.value.bus })
  with
  | Error e -> Or_error.error_s [%message "adding module inputs" (e : Error.t)]
  | Ok bus_map ->
    (match
       List.fold module_.cells ~init:(Ok bus_map) ~f:(fun map cell ->
         List.fold cell.outputs ~init:map ~f:(fun map output ->
           let%bind.Or_error map in
           add_cell_output map cell output))
     with
     | Error e -> Or_error.error_s [%message "adding cell outputs" (e : Error.t)]
     | Ok bus_map -> Ok bus_map)
;;
