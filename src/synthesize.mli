(** Synthesize a Verilog design in json *)

open! Import

val convert_to_json_file
  :  params:Parameter.t list
  -> topname:string
  -> blackboxes:string list
  -> verilog:string list
  -> json_file:string
  -> unit Or_error.t

val convert_to_json_string
  :  params:Parameter.t list
  -> topname:string
  -> blackboxes:string list
  -> verilog:string list
  -> string Or_error.t

val convert_to_json_netlist
  :  params:Parameter.t list
  -> topname:string
  -> blackboxes:string list
  -> verilog:string list
  -> Json_netlist.t Or_error.t
