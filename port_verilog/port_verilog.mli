open Core

module Make (X : sig
    val verilog_design : Hardcaml_of_verilog.Verilog_design.t
    val hardcaml_circuit : Hardcaml.Circuit.t
  end) : sig
  val command : Command.t
end
