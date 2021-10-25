open Base

val yosys_script : ?passes:Pass.t list -> Verilog_design.t -> json_file:string -> string

val to_json_file
  :  ?verbose:bool
  -> ?passes:Pass.t list
  -> Verilog_design.t
  -> json_file:string
  -> unit Or_error.t

val to_yosys_netlist
  :  ?verbose:bool
  -> ?passes:Pass.t list
  -> Verilog_design.t
  -> Yosys_netlist.t Or_error.t
