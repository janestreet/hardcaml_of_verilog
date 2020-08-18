open Base
open Stdio
module Parameter = Hardcaml.Parameter.Unstable

module Simple_parameter = struct
  type t = Hardcaml.Parameter_name.t * Hardcaml.Parameter.Value.Unstable.t
  [@@deriving sexp]
end

type 'param t_param =
  { name : string
  ; path : string
  ; instantiates : 'param t_param list
  ; params : 'param list
  }
[@@deriving compare, sexp]

(* This is simply so we can write [(N (Int 16))] instead of [((name M) (value (Int 16)))]
   in the sexps which define a verilog design parameter. *)
module T_simple = struct
  type t = Simple_parameter.t t_param [@@deriving sexp]

  let rec to_simple (t : Parameter.t t_param) : t =
    { name = t.name
    ; path = t.path
    ; instantiates = List.map t.instantiates ~f:to_simple
    ; params = List.map t.params ~f:(fun { name; value } -> name, value)
    }
  ;;

  let rec of_simple (t : t) : Parameter.t t_param =
    { name = t.name
    ; path = t.path
    ; instantiates = List.map t.instantiates ~f:of_simple
    ; params =
        List.map t.params ~f:(fun (name, value) -> Hardcaml.Parameter.{ name; value })
    }
  ;;
end

type t = Parameter.t t_param [@@deriving compare]

module File_io = struct
  type t =
    | At_file_path of string
    | Path_to_data of (string -> string)

  let ( ^/ ) = Caml.Filename.concat

  (* Ensure files are accessible on the filesystem. Creates temporary files if required.
  *)
  let on_filesystem file_io file_name =
    match file_io with
    | At_file_path prefix -> prefix ^/ file_name, Fn.id
    | Path_to_data f ->
      let temp_file_name = Caml.Filename.temp_file "yosys" ".v" in
      Out_channel.write_all temp_file_name ~data:(f file_name);
      temp_file_name, fun () -> Unix.unlink temp_file_name
  ;;
end

let sexp_of_t (t : t) = T_simple.sexp_of_t (T_simple.to_simple t)
let t_of_sexp sexp = T_simple.t_of_sexp sexp |> T_simple.of_simple
let equal = [%compare.equal: t]
let ( ^: ) a b = if String.is_empty a then b else Caml.Filename.concat a b

let create ?file ?(instantiates = []) ?(params = []) ?(path = "") () ~name =
  { name; path = path ^: Option.value file ~default:name ^ ".v"; instantiates; params }
;;

let collect_verilog_files ~blackbox design =
  (* ensure each verilog file is seen only once, the first time it is referenced *)
  let first_occurrence_only designs =
    let designs, _ =
      List.fold
        designs
        ~init:([], Set.empty (module String))
        ~f:(fun (ac, seen) (design : t) ->
          if Set.mem seen design.path
          then ac, seen
          else design.path :: ac, Set.add seen design.path)
    in
    List.rev designs
  in
  (* get all files in design *)
  let collect_hierarchy design =
    let rec collect (design : t) =
      let insts = List.concat @@ List.map ~f:collect design.instantiates in
      design :: insts
    in
    (* We [List.rev] here so that module definitions are processed before module
       uses. *)
    let verilog = first_occurrence_only @@ List.rev @@ collect design in
    verilog, []
  in
  (* find black boxes for top level module *)
  let collect_first_level_only (design : t) =
    let verilog = design.path in
    let blackboxes = first_occurrence_only design.instantiates in
    [ verilog ], blackboxes
  in
  if blackbox then collect_first_level_only design else collect_hierarchy design
;;

let to_json ?(file_io = File_io.At_file_path "") ~blackbox (design : t) =
  let verilog, blackboxes = collect_verilog_files ~blackbox design in
  (* Ensure the verilog files are stored on the filesystem somewhere (this is required by
     yosys). Clean up any temporary files created. *)
  let verilog, blackboxes, cleanup =
    let files f =
      List.fold f ~init:([], Fn.id) ~f:(fun (fs, clean) f ->
        let f, c = File_io.on_filesystem file_io f in
        ( f :: fs
        , fun () ->
          c ();
          clean () ))
    in
    let verilog, clean_verilog = files verilog in
    let blackboxes, clean_blackboxes = files blackboxes in
    ( verilog
    , blackboxes
    , fun () ->
      clean_verilog ();
      clean_blackboxes () )
  in
  let json =
    Synthesize.convert_to_json_string
      ~params:design.params
      ~blackboxes
      ~verilog
      ~topname:design.name
  in
  cleanup ();
  Or_error.ok_exn json
;;

let of_json ?topname ?use_netlist_names json_string =
  let json_netlist = Json_netlist.of_string json_string in
  let synthesized_designs =
    Or_error.ok_exn
      (Synthesized_designs.of_json_netlist
         json_netlist
         ~techlib:Techlib.Simlib.cells
         ?use_netlist_names)
  in
  match topname with
  | Some topname -> topname, Map.find_exn synthesized_designs topname
  | None ->
    (match Synthesized_designs.length synthesized_designs with
     | 0 -> failwith "no design loaded"
     | 1 -> Map.min_elt_exn synthesized_designs
     | _ -> failwith "multiple designs loaded")
;;

let load ?file_io ~blackbox verilog_design =
  let json_string =
    let json_string = to_json ?file_io verilog_design ~blackbox in
    json_string
  in
  let _, synthesized_design = of_json ~topname:verilog_design.name json_string in
  synthesized_design
;;
