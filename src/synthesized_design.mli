(** Represention of a design synthesized by yosys and converted to hardcaml *)

open! Import
open Hardcaml

type t [@@deriving sexp_of]
type 'a ports = (string * 'a) list [@@deriving sexp_of]

val create
  :  name:string
  -> inputs:int ports
  -> outputs:int ports
  -> (Signal.t ports -> Signal.t ports)
  -> t

val name : t -> string

(** name and width of input ports *)
val inputs : t -> int ports

(** name and width of output ports *)
val outputs : t -> int ports

(** function to construct the hardcaml representation of the synthesized design *)
val create_fn : t -> Signal.t ports -> Signal.t ports
