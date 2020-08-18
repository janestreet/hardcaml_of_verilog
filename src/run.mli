(** Running the Yosys. *)

open! Import

val yosys_exe : string

(** [with_args] returns a bash command that will run yosys with given arguments. With
    [~verbose:false], the command will run silently. *)
val with_args : ?verbose:bool (** default is [true] *) -> unit -> args:string -> string

(** [with_script] returns a bash command that will run yosys on the supplied
    [script_file]. With [~verbose:false], the command will run silently. *)
val with_script
  :  ?verbose:bool (** default is [true] *)
  -> unit
  -> script_file:string
  -> string
