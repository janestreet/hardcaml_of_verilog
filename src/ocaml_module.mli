(** Static construction of an ocaml module with hardcaml interfaces that dynamically loads
    the implementation at runtime. Interface widths are adjusted based on instantiation
    parameters. *)

open Hardcaml

module Rebuild_interfaces
    (I : Interface.S)
    (O : Interface.S) (X : sig
                         val loaded_design : Synthesized_design.t
                       end) : sig
  module I : Interface.S with type 'a t = 'a I.t
  module O : Interface.S with type 'a t = 'a O.t

  val create : Signal.t Interface.Create_fn(I)(O).t
end

val to_ocaml : Verilog_design.t -> Synthesized_design.t -> string
val save_ocaml : Verilog_design.t -> Synthesized_design.t -> file:string -> unit
