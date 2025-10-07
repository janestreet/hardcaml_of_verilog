open Base

module Parameter : sig
  type t = Hardcaml.Parameter.t [@@deriving sexp, equal ~localize]

  val create : name:string -> value:Hardcaml.Parameter.Value.t -> t
  val name : t -> string
  val value : t -> Hardcaml.Parameter.Value.t
  val string_of_value : t -> string
end

module Parameters : sig
  type t = Parameter.t list [@@deriving sexp]

  val replace : t -> with_:t -> t
end

module Define_value : sig
  type t =
    | String of string
    | Int of int
    | No_arg
  [@@deriving sexp, equal ~localize]

  val to_string : t -> string
end

module Define : sig
  type t [@@deriving sexp, equal ~localize]

  val create : name:string -> value:Define_value.t -> t
  val name : t -> string
  val value : t -> Define_value.t
end

module Defines : sig
  type t = Define.t list [@@deriving sexp]
end

module Module : sig
  type t [@@deriving sexp]

  val create
    :  ?blackbox:bool
    -> ?parameters:Parameters.t
    -> ?instantiates:t list
    -> module_name:string
    -> path:string
    -> unit
    -> t

  val override
    :  ?module_name:string
    -> ?path:string
    -> ?instantiates:t list
    -> ?parameters:Parameters.t
    -> ?blackbox:bool
    -> t
    -> t

  val blackbox : t -> bool
  val parameters : t -> Parameters.t
  val module_name : t -> string
  val path : t -> string
  val instantiates : t -> t list

  (** {2 Iterators}

      Depth first and call [f] from the leaves towards the root of the hierarchy. *)

  val iter : t -> f:(t -> unit) -> unit
  val map : t -> f:(t -> t) -> t

  (** Convert to a list. The "top" of the design is at the head of the list. *)
  val flat_map : t -> f:(t -> 'a) -> 'a list
end

type t [@@deriving sexp]

val create : ?defines:Defines.t -> top:Module.t -> unit -> t
val defines : t -> Defines.t
val top : t -> Module.t

(** Name of top level module *)
val top_name : t -> string

(** Override the parameters of the top level module *)
val override_parameters : t -> Parameters.t -> t

(** [map_paths t ~f] applies [f] to each modules path *)
val map_paths : t -> f:(string -> string) -> t

module type Crunched = sig
  val read : string -> string option
end

(** Read verilog files from [ocaml-cruch]ed file system(s) and extract to temp files. *)
val map_crunched_paths : ?delete_temp_files:bool -> (module Crunched) list -> t -> t

module type Embedded_files = sig
  val by_filename : (string * string) list
end

(** Read verilog files from [embed_file] file system(s) and extract to temp files. *)
val map_embed_file_paths
  :  ?delete_temp_files:bool
  -> (module Embedded_files) list
  -> t
  -> t
