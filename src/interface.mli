open! Import
open Hardcaml

module Make (I : Interface.S) (O : Interface.S) : sig
  val convert_exn
    :  ?use_netlist_names:bool
    -> unit
    -> blackboxes:string list
    -> topname:string
    -> verilog:string list
    -> (Signal.t I.t -> Signal.t O.t) Staged.t
end
