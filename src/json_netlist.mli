(** A Verilog design synthesized by Yosys to a json netlist. *)

open! Import

type t = Yosys_atd_t.t

val of_string : string -> t
val from_channel : In_channel.t -> t
val from_file : string -> t
