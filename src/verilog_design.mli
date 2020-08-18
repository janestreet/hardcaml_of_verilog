(** A description of a (hierachical) Verilog design, along with functions to convert it to
    json and a hardcaml design. *)

open! Base
open Hardcaml

type 'param t_param =
  { name : string (** Top-level name of verilog design  *)
  ; path : string (** [path] of verilog file. *)
  ; instantiates : 'param t_param list
  (** Sub-modules instantiated in the verilog design *)
  ; params : 'param list (** Top-level verilog module parameters *)
  }
[@@deriving compare, sexp]

type t = Parameter.t t_param [@@deriving compare, sexp]

(** Modify how files are found. Either via an adjusted path, or from a callback which
    stores file data in some custom fashion (ie ocaml-crunch). *)
module File_io : sig
  type t =
    | At_file_path of string
    (** [At_file_path prefix] will resolve to [prefix/file_path]. *)
    | Path_to_data of (string -> string)
    (** [Path_to_data f] applies [f file_path] which should return the file contents. *)
end

include Equal.S with type t := t

val create
  :  ?file:string
  -> ?instantiates:t list
  -> ?params:Parameter.t list
  -> ?path:string
  -> unit
  -> name:string
  -> t

val to_json : ?file_io:File_io.t -> blackbox:bool -> t -> string

val of_json
  :  ?topname:string
  -> ?use_netlist_names:bool
  -> string
  -> string * Synthesized_design.t

val load : ?file_io:File_io.t -> blackbox:bool -> t -> Synthesized_design.t
