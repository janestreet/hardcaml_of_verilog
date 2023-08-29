(** Netlist representing a verilog design synthesized with yosys.

    The conversion is done lazily when a module is referenced. *)

open Base
module Bit = Yosys_netlist.Bit

module Bus : sig
  type t = Bit.t list [@@deriving sexp_of]

  include Comparator.S with type t := t
end

module Parameter : sig
  type t = Hardcaml.Parameter.t [@@deriving sexp_of]
end

module Port : sig
  type 'a t =
    { name : string
    ; value : 'a
    }
  [@@deriving sexp_of, fields ~getters]

  val find : 'a t list -> string -> 'a t Or_error.t
  val find_exn : 'a t list -> string -> 'a t
end

module Cell : sig
  type t =
    { module_name : string
    ; instance_name : string
    ; parameters : Parameter.t list
    ; inputs : Bus.t Port.t list
    ; outputs : Bus.t Port.t list
    }
  [@@deriving sexp_of]
end

module Bus_names : sig
  type t [@@deriving sexp_of]

  val create : Yosys_netlist.Netname.t list -> t
  val find : t -> Bus.t -> string list
end

module Module : sig
  type t =
    { name : string
    ; inputs : Bus.t Port.t list
    ; outputs : Bus.t Port.t list
    ; cells : Cell.t list
    ; bus_names : Bus_names.t
    }
  [@@deriving sexp_of]

  val sanitize_instance_names : t -> t
end

type t [@@deriving sexp_of]

(** Synthesize a [Verilog_design] into a netlist. *)
val create : ?verbose:bool -> ?passes:Pass.t list -> Verilog_design.t -> t Or_error.t

(** Get a module from the netlist by module name. Note that conversion actally happens on
    lookup, hence the error return (this allows us to selectively convert modules from a
    netlist, without needing all of them to be correct). *)
val find_module_by_name : t -> string -> Module.t Or_error.t

(** Return all modules in the netlist *)
val get_all_modules : t -> Module.t list Or_error.t

(** Convert a raw yosys netlist. *)
val of_yosys_netlist : Yosys_netlist.t -> t Or_error.t
