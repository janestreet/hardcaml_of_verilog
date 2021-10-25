open Base

type t =
  | Proc
  | Flatten
  | Memory of { nomap : bool }
  | Opt of { mux_undef : bool }
  | Clean
[@@deriving sexp_of, equal]

val to_string : t -> string
