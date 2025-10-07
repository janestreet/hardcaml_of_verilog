open Base
open Jsonaf.Export

module Int_or_string = struct
  type t =
    | Int of int
    | String of string
  [@@deriving sexp_of]

  let t_of_jsonaf t =
    match Jsonaf.int t with
    | Some i -> Int i
    | None ->
      (match Jsonaf.string t with
       | Some s -> String s
       | None -> raise_s [%message "Int or String value expected" (t : Jsonaf.t)])
  ;;

  let jsonaf_of_t = function
    | Int i -> jsonaf_of_int i
    | String s -> jsonaf_of_string s
  ;;
end

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
  end) =
struct
  type t =
    { name : string
    ; value : V.t
    }
  [@@deriving sexp_of]

  module List = struct
    type nonrec t = t list [@@deriving sexp_of]

    let t_of_jsonaf t : t =
      Jsonaf.assoc_list_exn t
      |> List.map ~f:(fun (name, v) -> { name; value = V.t_of_jsonaf v })
    ;;

    let jsonaf_of_t (t : t) =
      `Object (List.map t ~f:(fun t -> t.name, V.jsonaf_of_t t.value))
    ;;
  end
end

module Direction = struct
  type t =
    | Input
    | Output
  [@@deriving sexp_of, equal ~localize]

  let t_of_jsonaf t =
    match Jsonaf.string t with
    | None -> raise_s [%message "Expecting JSON string for direction"]
    | Some "input" -> Input
    | Some "output" -> Output
    | _ as direction -> raise_s [%message "Invalid direction" (direction : string option)]
  ;;

  let jsonaf_of_t = function
    | Input -> jsonaf_of_string "input"
    | Output -> jsonaf_of_string "output"
  ;;
end

module Bit = struct
  type t =
    | Vdd
    | Gnd
    | X
    | Index of int
  [@@deriving compare ~localize, sexp_of]

  let t_of_jsonaf t =
    match Int_or_string.t_of_jsonaf t with
    | String "0" -> Gnd
    | String "1" -> Vdd
    | String "x" | String "X" -> X
    | String s -> raise_s [%message "Invalid bit value" (s : string)]
    | Int i -> Index i
  ;;

  let jsonaf_of_t = function
    | Gnd -> jsonaf_of_string "0"
    | Vdd -> jsonaf_of_string "1"
    | X -> jsonaf_of_string "x"
    | Index i -> jsonaf_of_int i
  ;;
end

module Port = struct
  module V = struct
    type t =
      { direction : Direction.t
      ; bits : Bit.t list
      }
    [@@deriving jsonaf, sexp_of]
  end

  include Assoc_list (V)
end

module Parameter = struct
  module V = struct
    type t = Int_or_string.t [@@deriving jsonaf, sexp_of]
  end

  include Assoc_list (V)
end

module Connection = struct
  module V = struct
    type t = Bit.t list [@@deriving jsonaf, sexp_of]
  end

  include Assoc_list (V)
end

module Port_direction = struct
  module V = struct
    type t = Direction.t [@@deriving jsonaf, sexp_of]
  end

  include Assoc_list (V)
end

module Cell = struct
  module V = struct
    type t =
      { hide_name : int [@jsonaf.default 1]
      ; module_name : string [@key "type"]
      ; parameters : Parameter.List.t [@jsonaf.default []]
      ; port_directions : Port_direction.List.t [@jsonaf.default []]
      ; connections : Connection.List.t [@jsonaf.default []]
      }
    [@@deriving jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
  end

  include Assoc_list (V)
end

module Netname = struct
  module V = struct
    type t =
      { hide_name : int
      ; bits : Bit.t list
      }
    [@@deriving jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
  end

  include Assoc_list (V)
end

module Module = struct
  module V = struct
    type t =
      { ports : Port.List.t [@jsonaf.default []]
      ; cells : Cell.List.t [@jsonaf.default []]
      ; netnames : Netname.List.t [@jsonaf.default []]
      }
    [@@deriving jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
  end

  include Assoc_list (V)
end

type t =
  { creator : string
  ; modules : Module.List.t
  }
[@@deriving jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]

let of_string s =
  let%bind.Or_error json = Jsonaf.parse s in
  Or_error.try_with (fun () -> t_of_jsonaf json)
;;

let to_string n = Jsonaf.to_string (jsonaf_of_t n)
let to_string_hum n = Jsonaf.to_string_hum (jsonaf_of_t n)

let find_module_by_name t name =
  List.find t.modules ~f:(fun m -> String.equal name m.name)
;;
