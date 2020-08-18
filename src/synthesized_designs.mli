(** Map of hardcaml designs synthesized from verilog by yosys, converted to a json
    representation and reconstructed by this library. *)

open! Import

type t = Synthesized_design.t Map.M(String).t [@@deriving sexp_of]

val length : t -> int

(** Function to convert a json formatted netlist produced by yosys into a map of
    synthesized designs. If [allow_blackboxes] is true any [Cell_reference.t] not in the
    [techlib] will generate an instantiation, otherwise the conversion fails. When
    [use_netlist_names] is true a best guess mapping of names to hardcaml signals will be
    performed. *)
type 'json_netlist synthesized_design_converter =
  ?allow_blackboxes:bool (** default is [true] *)
  -> ?use_netlist_names:bool (** default is [true] *)
  -> techlib:Techlib.t
  -> 'json_netlist
  -> t Or_error.t

(** Same as [synthesized_design_converter] but raises exceptions on error *)
type 'json_netlist synthesized_design_converter_exn =
  ?allow_blackboxes:bool (** default is [true] *)
  -> ?use_netlist_names:bool (** default is [true] *)
  -> techlib:Techlib.t
  -> 'json_netlist
  -> t

(** Convert a json netlist to [t] *)
val of_json_netlist : Json_netlist.t synthesized_design_converter

val of_json_netlist_exn : Json_netlist.t synthesized_design_converter_exn
