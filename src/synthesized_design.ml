open! Import

type t =
  { name : string
  ; inputs : (string * int) list
  ; outputs : (string * int) list
  ; create_fn :
      ((string * Hardcaml.Signal.t) list -> (string * Hardcaml.Signal.t) list
       [@sexp.opaque])
  }
[@@deriving fields, sexp_of]

type 'a ports = (string * 'a) list [@@deriving sexp_of]

let create ~name ~inputs ~outputs create_fn = { name; inputs; outputs; create_fn }
