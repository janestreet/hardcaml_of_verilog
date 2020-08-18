open! Import

type bits = int list

type t =
  { typ : string
  ; label : string
  ; parameters : (string * Yosys_atd_t.param_value) list
  ; inputs : (string * bits) list
  ; outputs : (string * bits) list
  }

exception No_cell_direction_specified of string * string
exception Invalid_net_tristate
exception Invalid_net_bit_specifier of string
exception Unsupported_parameter_type of string * string

val net_of_bit : Yosys_atd_t.dyn -> int

val create_params
  :  string
  -> (string * Yosys_atd_t.param_value) list
  -> Hardcaml.Parameter.t list

val partition_ios
  :  Yosys_atd_t.cell
  -> (string * Yosys_atd_t.bits) list * (string * Yosys_atd_t.bits) list

val create_cell : Yosys_atd_t.cell -> t
