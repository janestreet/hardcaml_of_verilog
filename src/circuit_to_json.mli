(** Takes in a Hardcaml circuit and transforms it to a json file that can be read and
    rendered by netlistsvg. *)

open Base

val convert : ?debug:bool -> Hardcaml.Circuit.t -> Yosys_netlist.t
