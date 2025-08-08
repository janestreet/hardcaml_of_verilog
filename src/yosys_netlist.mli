(** Raw json netlist representation parsed with [jsonaf]. See [Netlist] for a cleaned up
    version. *)

open Base

module type Assoc_list = sig
  type v

  type t =
    { name : string
    ; value : v
    }
  [@@deriving sexp_of]
end

module Assoc_list (V : sig
    type t [@@deriving jsonaf, sexp_of]
  end) : Assoc_list with type v := V.t

module Int_or_string : sig
  type t =
    | Int of int
    | String of string
  [@@deriving jsonaf, sexp_of]
end

module Direction : sig
  type t =
    | Input
    | Output
  [@@deriving equal ~localize, jsonaf, sexp_of]
end

module Bit : sig
  type t =
    | Vdd
    | Gnd
    | X
    | Index of int
  [@@deriving compare ~localize, jsonaf, sexp_of]
end

module Port : sig
  module V : sig
    type t =
      { direction : Direction.t
      ; bits : Bit.t list
      }
    [@@deriving jsonaf, sexp_of]
  end

  include Assoc_list with type v := V.t
end

module Parameter : sig
  module V : sig
    type t = Int_or_string.t [@@deriving jsonaf, sexp_of]
  end

  include Assoc_list with type v := V.t
end

module Connection : sig
  module V : sig
    type t = Bit.t list [@@deriving jsonaf, sexp_of]
  end

  include Assoc_list with type v := V.t
end

module Port_direction : sig
  module V : sig
    type t = Direction.t [@@deriving jsonaf, sexp_of]
  end

  include Assoc_list with type v := V.t
end

module Cell : sig
  module V : sig
    type t =
      { hide_name : int
      ; module_name : string
      ; parameters : Parameter.t list
      ; port_directions : Port_direction.t list
      ; connections : Connection.t list
      }
    [@@deriving jsonaf, sexp_of]
  end

  include Assoc_list with type v := V.t
end

module Netname : sig
  module V : sig
    type t =
      { hide_name : int
      ; bits : Bit.t list
      }
  end

  include Assoc_list with type v := V.t
end

module Module : sig
  module V : sig
    type t =
      { ports : Port.t list
      ; cells : Cell.t list
      ; netnames : Netname.t list
      }
    [@@deriving jsonaf, sexp_of]
  end

  include Assoc_list with type v := V.t
end

type t =
  { creator : string
  ; modules : Module.t list
  }
[@@deriving jsonaf, sexp_of]

(** Parse a string containing json into a [Yosys_netlist]. *)
val of_string : string -> t Or_error.t

(** Convert a [Yosys_netlist] to a json string ([_hum] is with indentation) *)
val to_string : t -> string

val to_string_hum : t -> string

(** Lookup a module in the netlist. *)
val find_module_by_name : t -> string -> Module.t option
