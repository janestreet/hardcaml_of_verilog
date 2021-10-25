open Base

module Make (I : Hardcaml.Interface.S) (O : Hardcaml.Interface.S) : sig
  val create
    :  ?verbose:bool
    -> ?passes:Pass.t list
    -> Verilog_design.t
    -> (Hardcaml.Signal.t I.t -> Hardcaml.Signal.t O.t Or_error.t) Or_error.t
end
