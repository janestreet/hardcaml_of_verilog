open Base
open Hardcaml
module Cell = Netlist.Cell
module Port = Netlist.Port

module Cell_implementation : sig
  type create_fn =
    Cell.t -> Parameter.t list -> Signal.t Port.t list -> Signal.t Port.t list Or_error.t

  type t =
    { name : string
    ; create_fn : create_fn
    }
end

(** Names of black boxes in the techlib. These are cells we haven't yet implemented. *)
val blackboxes : string list

(** List of cells in the techlib *)
val cells : Cell_implementation.t list

(** Find a cell in the techlib *)
val find : Cell_implementation.t list -> string -> Cell_implementation.create_fn option
