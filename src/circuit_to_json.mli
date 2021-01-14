(** Takes in a Hardcaml circuit and transforms it to a json file that can be read and
    rendered by netlistsvg. *)

val convert : ?debug:bool -> Hardcaml.Circuit.t -> Yosys_atd_t.t
val to_string : Yosys_atd_t.t -> string
