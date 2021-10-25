(** Internal module. See comment at the top of its implementation for more information. *)

open Base
module Bus = Netlist.Bus
module Cell = Netlist.Cell
module Port = Netlist.Port

type t

val create
  :  Netlist.Module.t
  -> circuit_inputs:Hardcaml.Signal.t Port.t list
  -> t Or_error.t

val signal_of_bus : t -> Bus.t Port.t -> Hardcaml.Signal.t Port.t Or_error.t

val signal_of_bus_if_not_empty
  :  t
  -> Bus.t Port.t
  -> Hardcaml.Signal.t Port.t Or_error.t Option.t

val wire_of_cell_output
  :  t
  -> Cell.t
  -> Bus.t Port.t
  -> Hardcaml.Signal.t Port.t Or_error.t Option.t
