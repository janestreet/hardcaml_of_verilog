open! Import
module Y = Yosys_atd_t

type bits = int list

type t =
  { typ : string
  ; label : string
  ; parameters : (string * Y.param_value) list
  ; inputs : (string * bits) list
  ; outputs : (string * bits) list
  }

exception No_cell_direction_specified of string * string
exception Invalid_net_tristate
exception Invalid_net_bit_specifier of string
exception Unsupported_parameter_type of string * string

let net_of_bit = function
  | `Int i -> i
  | `String "x" | `String "X" -> 0
  | `String "z" | `String "Z" -> raise Invalid_net_tristate
  | `String x ->
    (try Int.of_string x with
     | _ -> raise (Invalid_net_bit_specifier x))
  | _ -> raise (Invalid_net_bit_specifier "unknown json type")
;;

let create_params cell_name parameters =
  List.map parameters ~f:(fun (name, value) ->
    Hardcaml.Parameter.create
      ~name
      ~value:
        (match value with
         | `Int i -> Int i
         | `String s -> String s
         | _ -> raise (Unsupported_parameter_type (cell_name, name))))
;;

let partition_ios cell =
  List.partition_tf cell.Y.connections ~f:(fun (n, _) ->
    match List.Assoc.find cell.Y.port_directions n ~equal:Poly.equal with
    | Some `Input -> true
    | Some _ -> false
    | None -> raise (No_cell_direction_specified (cell.Y.typ, n)))
;;

let create_cell cell =
  let inputs, outputs = partition_ios cell in
  let port (name, bits) = name, List.map bits ~f:net_of_bit in
  let inputs, outputs = List.map ~f:port inputs, List.map ~f:port outputs in
  { typ = cell.Y.typ
  ; label = Y.(cell.attributes.src)
  ; parameters = cell.Y.parameters
  ; inputs
  ; outputs
  }
;;
