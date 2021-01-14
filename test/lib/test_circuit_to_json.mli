(** An example of a simple adder in Hardcaml. *)
open Hardcaml

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; a : 'a
    ; b : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t = { y : 'a } [@@deriving sexp_of, hardcaml]
end

val create : Signal.t I.t -> Signal.t O.t
