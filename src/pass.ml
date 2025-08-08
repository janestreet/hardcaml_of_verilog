open Base

type t =
  | Proc
  | Flatten
  | Memory of { nomap : bool }
  | Opt of { mux_undef : bool }
  | Clean
[@@deriving sexp_of, equal ~localize]

let to_string = function
  | Proc -> "proc"
  | Flatten -> "flatten"
  | Memory { nomap } ->
    let nomap = if nomap then " -nomap" else "" in
    "memory" ^ nomap
  | Opt { mux_undef } ->
    let mux_undef = if mux_undef then " -mux_undef" else "" in
    "opt" ^ mux_undef
  | Clean -> "clean"
;;
