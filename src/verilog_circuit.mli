(** A data structure representing the hardcaml implementation of a [Verilog_design.t]
    converted to a [Netlist.t]. *)

open Base
module Port = Netlist.Port

type t [@@deriving sexp_of]

val create : Netlist.t -> top_name:string -> t Or_error.t
val inputs : t -> int Port.t list
val outputs : t -> int Port.t list

val create_fn
  :  t
  -> Hardcaml.Signal.t Port.t list
  -> Hardcaml.Signal.t Port.t list Or_error.t

val to_hardcaml_circuit : t -> Hardcaml.Circuit.t Or_error.t
