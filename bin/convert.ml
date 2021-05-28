(* convert verilog to/from json using yosys. *)

open Core
open Hardcaml
open Hardcaml_of_verilog

let load_json_and_save_rtl ~black_box ~keep_names ~vhdl ~core_name ~json_file =
  let fns =
    match json_file with
    | Some json_file ->
      Synthesized_designs.of_json_netlist_exn
        ~allow_blackboxes:black_box
        ~use_netlist_names:keep_names
        ~techlib:Techlib.Simlib.cells
        (Json_netlist.from_file json_file)
    | None ->
      Synthesized_designs.of_json_netlist_exn
        ~allow_blackboxes:black_box
        ~use_netlist_names:keep_names
        ~techlib:Techlib.Simlib.cells
        (Json_netlist.from_channel In_channel.stdin)
  in
  let design =
    match core_name with
    | Some core_name -> Map.find_exn fns core_name
    | None -> snd (Map.min_elt_exn fns)
  in
  let inputs =
    List.map (Synthesized_design.inputs design) ~f:(fun (n, b) -> n, Signal.input n b)
  in
  let outputs = Synthesized_design.create_fn design inputs in
  let circ =
    Circuit.create_exn
      ~name:(Synthesized_design.name design)
      (List.map outputs ~f:(fun (n, s) -> Signal.output n s))
  in
  if vhdl then Rtl.print Vhdl circ else Rtl.print Verilog circ
;;

let to_rtl_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Convert a single design in a JSON file to VHDL or Verilog"
    [%map_open
      let json_file =
        flag "-json" (optional string) ~doc:"JSON Input JSON file.  Use stdin by default"
      and vhdl = flag "-vhdl" no_arg ~doc:" Convert to VHDL.  Default is Verilog"
      and core_name = flag "-design" (optional string) ~doc:"NAME Design to extract"
      and black_box = flag "-black-box" no_arg ~doc:"Allow black boxes in the netlist"
      and keep_names =
        flag "-keep-names" no_arg ~doc:"Try to keep names specified in the netlist"
      in
      fun () -> load_json_and_save_rtl ~black_box ~keep_names ~vhdl ~core_name ~json_file]
;;

let output_string_opt file ~data =
  match file with
  | Some file -> Out_channel.write_all file ~data
  | None -> Out_channel.print_string data
;;

let convert_verilog_to_json ~blackboxes ~verilog ~json_file ~topname =
  let json =
    ok_exn (Synthesize.convert_to_json_string ~params:[] ~blackboxes ~verilog ~topname)
  in
  output_string_opt json_file ~data:json
;;

let to_json_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Convert a design in a (single) Verilog file to json."
    [%map_open
      let json_file =
        flag
          "-json"
          (optional string)
          ~doc:"JSON Output JSON file.  Use stdout by default"
      and verilog_file = flag "-verilog" (required string) ~doc:"FILE Input verilog file"
      and topname = flag "-top" (required string) ~doc:"TOP Top level design name" in
      fun () ->
        convert_verilog_to_json
          ~blackboxes:[]
          ~verilog:[ verilog_file ]
          ~json_file
          ~topname]
;;

let load_sexp s =
  try Sexp.load_sexp s with
  | _ -> Sexp.of_string s
;;

let verilog_design_json_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Convert a [Verilog_design.t] to JSON."
    [%map_open
      let verilog_design =
        flag
          "-design"
          (required string)
          ~doc:"SEXP Sexp (or file containing a sexp) of a [Verilog_design.t]"
      and json_file =
        flag
          "-json-file"
          (optional string)
          ~doc:"JSON Ouput JSON file.  stdout by default"
      and path =
        flag "-path" (optional string) ~doc:"PATH Prefix of path to verilog files"
      and blackbox = flag "-blackbox" no_arg ~doc:"Allow blackboxes." in
      fun () ->
        let verilog_design = Verilog_design.t_of_sexp (load_sexp verilog_design) in
        let json =
          Verilog_design.to_json
            ?file_io:
              (Option.map path ~f:(fun path -> Verilog_design.File_io.At_file_path path))
            ~blackbox
            verilog_design
        in
        output_string_opt json_file ~data:json]
;;

let ocaml_module_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Convert a [Verilog_design.t] to an OCaml module."
    [%map_open
      let verilog_design =
        flag
          "-design"
          (required string)
          ~doc:"SEXP Sexp (or file containing a sexp) of a [Verilog_design.t]"
      and json_file =
        flag
          "-json-file"
          (optional string)
          ~doc:
            "JSON JSON netlist.  If unspecified the Verilog design is synthesized with \
             yosys."
      and ocaml_module =
        flag
          "-ocaml-module"
          (optional string)
          ~doc:"OCAML OCaml output file.  stdout by default"
      and path =
        flag "-path" (optional string) ~doc:"PATH Prefix of path to verilog files"
      in
      fun () ->
        let verilog_design = Verilog_design.t_of_sexp (load_sexp verilog_design) in
        let synthesized_design =
          match json_file with
          | None ->
            Verilog_design.load
              ?file_io:
                (Option.map path ~f:(fun path -> Verilog_design.File_io.At_file_path path))
              ~blackbox:true
              verilog_design
          | Some json_file ->
            Verilog_design.of_json ~topname:verilog_design.name json_file |> snd
        in
        let ocaml = Ocaml_module.to_ocaml verilog_design synthesized_design in
        output_string_opt ocaml_module ~data:ocaml]
;;

let () =
  Command.group
    ~summary:""
    [ ( "simple"
      , Command.group
          ~summary:"Onshot conversion of a single verilog file with yosys"
          [ "json", to_json_command; "rtl", to_rtl_command ] )
    ; ( "verilog-design"
      , Command.group
          ~summary:"Conversion of a [Verilog_design.t] to JSON and OCaml."
          [ "json", verilog_design_json_command; "ocaml-module", ocaml_module_command ] )
    ]
  |> Command_unix.run
;;
