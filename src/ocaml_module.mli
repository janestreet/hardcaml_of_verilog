(** Static construction of an ocaml module with hardcaml interfaces that dynamically loads
    the implementation at runtime. Interface widths are adjusted based on instantiation
    parameters. *)

open Base

module Rebuild_interfaces
    (I : Hardcaml.Interface.S)
    (O : Hardcaml.Interface.S)
    (X : sig
       val verilog_design : Verilog_design.t
       val loaded_design : Verilog_circuit.t
     end) : sig
  val verilog_design : Verilog_design.t

  module I : Hardcaml.Interface.S with type 'a t = 'a I.t
  module O : Hardcaml.Interface.S with type 'a t = 'a O.t

  val create : Hardcaml.Interface.Create_fn(I)(O).t
  val inst : ?name:string -> ?instance:string -> Hardcaml.Interface.Create_fn(I)(O).t

  val hierarchical
    :  ?name:string
    -> ?instance:string
    -> Hardcaml.Scope.t
    -> Hardcaml.Interface.Create_fn(I)(O).t
end

val to_ocaml : Verilog_design.t -> Verilog_circuit.t -> string
val save_ocaml : Verilog_design.t -> Verilog_circuit.t -> file:string -> unit
