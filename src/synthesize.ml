open Base
open Stdio
module V = Verilog_design
module M = V.Module

let default_passes =
  Pass.
    [ Proc
    ; Flatten
    ; Memory { nomap = true }
    ; Opt { mux_undef = false }
    ; Clean
    ; Opt { mux_undef = true }
    ; Clean
    ]
;;

let get_parameters top =
  match M.parameters top with
  | [] -> None
  | params ->
    List.map params ~f:(fun p ->
      let name = Verilog_design.Parameter.name p in
      let value = Verilog_design.Parameter.string_of_value p in
      [%string {|-set %{name} %{value}|}])
    |> String.concat ~sep:" "
    |> Option.some
;;

let get_defines top =
  V.defines top
  |> List.map ~f:(fun d ->
    let value = V.Define.value d in
    if V.Define_value.equal No_arg value
    then [%string {|-D%{V.Define.name d}|}]
    else [%string {|-D%{V.Define.name d}=%{V.Define_value.to_string value}|}])
  |> String.concat ~sep:" "
;;

let get_unique_files top predicate =
  let rec unique seen modules =
    match modules with
    | [] -> []
    | hd :: tl ->
      if Set.mem seen (M.path hd)
      then unique seen tl
      else hd :: unique (Set.add seen (M.path hd)) tl
  in
  M.flat_map top ~f:(fun m -> if predicate m then Some m else None)
  |> List.filter_opt
  |> unique (Set.empty (module String))
  |> List.rev
;;

let yosys_script ?(passes = default_passes) verilog_design ~json_file =
  let buffer = Buffer.create 1024 in
  let add line = Buffer.add_string buffer (line ^ "\n") in
  let top = V.top verilog_design in
  let params = get_parameters top in
  let defines = get_defines verilog_design in
  List.iter (get_unique_files top M.blackbox) ~f:(fun m ->
    add [%string {|read_verilog %{defines} -defer -lib %{M.path m}|}]);
  List.iter
    (get_unique_files top (Fn.non M.blackbox))
    ~f:(fun m -> add [%string {|read_verilog %{defines} -defer %{M.path m}|}]);
  (* It seems you must set all parameters at once, or otherwise it sets some, but not
     others, in non-untuitive ways. *)
  Option.iter params ~f:(fun params ->
    add [%string "chparam %{params} %{M.module_name top}"]);
  add [%string {|hierarchy -top %{M.module_name top}|}];
  List.iter passes ~f:(fun pass -> add (Pass.to_string pass));
  add [%string {|write_json %{json_file}|}];
  Buffer.contents buffer
;;

let tmp_file ~unlink ext =
  let name = Filename_unix.temp_file "hardcaml_of_verilog_synthesize_" ext in
  if unlink then Stdlib.at_exit (fun () -> Unix.unlink name);
  name
;;

let tmp_out_channel ~unlink ext =
  let name = tmp_file ~unlink ext in
  name, Out_channel.create name
;;

let write_tmp_yosys_script ?passes verilog_design ~json_file =
  let script_name, script = tmp_out_channel ~unlink:true ".yosys" in
  Out_channel.output_string script (yosys_script ?passes verilog_design ~json_file);
  Out_channel.close script;
  script_name
;;

let run_yosys ?(verbose = false) args =
  let verbose = if verbose then [] else [ "2>/dev/null"; ">/dev/null" ] in
  let command =
    String.concat
      ~sep:" "
      (List.concat [ [ Hardcaml.Tools_config.yosys ]; args; verbose ])
  in
  match Unix.system command with
  | WEXITED 0 -> Ok ()
  | _ -> Or_error.error_s [%message "YOSYS failed."]
;;

let to_json_file ?verbose ?passes verilog_design ~json_file =
  let script_name = write_tmp_yosys_script ?passes verilog_design ~json_file in
  run_yosys ?verbose [ "-s"; script_name ]
;;

let to_yosys_netlist ?verbose ?passes verilog_design =
  let json_file = tmp_file ~unlink:true ".json" in
  let%bind.Or_error () = to_json_file ?verbose ?passes verilog_design ~json_file in
  let%bind.Or_error json = Or_error.try_with (fun () -> In_channel.read_all json_file) in
  Yosys_netlist.of_string json
;;
