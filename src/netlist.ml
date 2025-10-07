open! Base
module Bit = Yosys_netlist.Bit

module Bus = struct
  module T = struct
    type t = Bit.t list [@@deriving sexp_of, compare ~localize]
  end

  include T
  include Comparator.Make (T)
end

module Parameter = struct
  type t = Hardcaml.Parameter.t [@@deriving sexp_of]

  let of_yosys_netlist (p : Yosys_netlist.Parameter.t) =
    match p.value with
    | Int i -> Hardcaml.Parameter.create ~name:p.name ~value:(Int i)
    | String s -> Hardcaml.Parameter.create ~name:p.name ~value:(String s)
  ;;
end

module Port = struct
  type 'a t =
    { name : string
    ; value : 'a
    }
  [@@deriving sexp_of, fields ~getters]

  let find (ports : _ t list) name =
    let rec f = function
      | [] ->
        Or_error.error_s
          [%message "Could not find port" (name : string) (ports : _ t list)]
      | h :: t -> if String.equal h.name name then Ok h else f t
    in
    f ports
  ;;

  let find_exn ports name = find ports name |> Or_error.ok_exn
end

module Cell = struct
  type t =
    { module_name : string
    ; instance_name : string
    ; parameters : Parameter.t list
    ; inputs : Bus.t Port.t list
    ; outputs : Bus.t Port.t list
    }
  [@@deriving sexp_of]

  let partition_by_port_direction
    (cell : Yosys_netlist.Cell.t)
    (directions : Yosys_netlist.Direction.t Map.M(String).t)
    =
    let rec f inputs outputs (connections : Yosys_netlist.Connection.t list) =
      match connections with
      | [] -> Ok (inputs, outputs)
      | conn :: conns ->
        (match Map.find directions conn.name with
         | None ->
           Or_error.error_s
             [%message
               "No port direction specified"
                 (conn : Yosys_netlist.Connection.t)
                 (cell : Yosys_netlist.Cell.t)]
         | Some Input ->
           f ({ Port.name = conn.name; value = conn.value } :: inputs) outputs conns
         | Some Output ->
           f inputs ({ Port.name = conn.name; value = conn.value } :: outputs) conns)
    in
    f [] [] cell.value.connections
  ;;

  let of_yosys_netlist (cell : Yosys_netlist.Cell.t) =
    let%bind.Or_error directions =
      List.fold
        cell.value.port_directions
        ~init:(Ok (Map.empty (module String)))
        ~f:(fun map dirn ->
          let%bind.Or_error map in
          match Map.add map ~key:dirn.name ~data:dirn.value with
          | `Ok map -> Ok map
          | `Duplicate ->
            Or_error.error_s
              [%message
                "Port direction specified more than once"
                  (dirn : Yosys_netlist.Port_direction.t)
                  (cell : Yosys_netlist.Cell.t)])
    in
    let%bind.Or_error inputs, outputs = partition_by_port_direction cell directions in
    Ok
      { module_name = cell.value.module_name
      ; instance_name = cell.name
      ; parameters = List.map cell.value.parameters ~f:Parameter.of_yosys_netlist
      ; inputs
      ; outputs
      }
  ;;
end

(* Map of bus names from net indexes. *)
module Bus_names = struct
  type t = string list Map.M(Bus).t [@@deriving sexp_of]

  let add_to map ~key ~data =
    match Map.find map key with
    | None ->
      (* this wont raise, as we just check the key doesn't exist. *)
      Map.add_exn map ~key ~data:[ data ]
    | Some lst -> (* replace the key *) Map.set map ~key ~data:(data :: lst)
  ;;

  let create (netnames : Yosys_netlist.Netname.t list) : t =
    List.fold
      netnames
      ~init:(Map.empty (module Bus))
      ~f:(fun map netname ->
        if netname.value.hide_name = 1
        then map
        else add_to map ~key:netname.value.bits ~data:netname.name)
  ;;

  let find t bus =
    match Map.find t bus with
    | None -> []
    | Some l -> l
  ;;
end

module Module = struct
  type t =
    { name : string
    ; inputs : Bus.t Port.t list
    ; outputs : Bus.t Port.t list
    ; cells : Cell.t list
    ; bus_names : Bus_names.t
    }
  [@@deriving sexp_of]

  (* Partition ports of module into inputs and outputs *)
  let partition_module_ports (module_ : Yosys_netlist.Module.t) =
    let inputs, outputs =
      List.partition_tf module_.value.ports ~f:(fun p ->
        Yosys_netlist.Direction.(equal Input p.value.direction))
    in
    let to_ports ports =
      List.map
        (ports : Yosys_netlist.Port.t list)
        ~f:(fun p -> { Port.name = p.name; value = p.value.bits })
    in
    to_ports inputs, to_ports outputs
  ;;

  let of_yosys_netlist name (t : Yosys_netlist.Module.t) =
    let inputs, outputs = partition_module_ports t in
    let%bind.Or_error cells =
      List.map t.value.cells ~f:Cell.of_yosys_netlist |> Or_error.all
    in
    Ok { name; inputs; outputs; cells; bus_names = Bus_names.create t.value.netnames }
  ;;

  let sanitize_instance_names module_ =
    { module_ with
      cells =
        List.mapi module_.cells ~f:(fun index cell ->
          { cell with instance_name = "the_inst_" ^ Int.to_string index })
    }
  ;;
end

type t = (string * Module.t Or_error.t Lazy.t) list

let sexp_of_t t = [%sexp_of: string list] (List.map t ~f:fst)

let of_yosys_netlist (t : Yosys_netlist.t) =
  Or_error.try_with (fun () ->
    List.map t.modules ~f:(fun module_ ->
      module_.name, lazy (Module.of_yosys_netlist module_.name module_)))
;;

let find_module_by_name t name =
  match List.Assoc.find t ~equal:String.equal name with
  | None -> Or_error.error_s [%message "Failed to find requested module" (name : string)]
  | Some module_ -> Lazy.force module_
;;

let get_all_modules t =
  List.map t ~f:(fun (_, module_) -> Lazy.force module_) |> Or_error.all
;;

let create ?verbose ?passes verilog_design =
  let%bind.Or_error netlist =
    Synthesize.to_yosys_netlist ?verbose ?passes verilog_design
  in
  of_yosys_netlist netlist
;;
