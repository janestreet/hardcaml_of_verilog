open Core
open Stdio
open Hardcaml_of_verilog

let in_chan =
  Command.Arg_type.create (fun n -> In_channel.create n)
  |> Command.Flag.optional_with_default In_channel.stdin
;;

let out_chan =
  Command.Arg_type.create Out_channel.create
  |> Command.Flag.optional_with_default Out_channel.stdout
;;

let rtl =
  Command.Arg_type.create (fun rtl ->
    match String.lowercase rtl with
    | "verilog" | "vlog" -> Hardcaml.Rtl.Language.Verilog
    | "vhdl" -> Hardcaml.Rtl.Language.Vhdl
    | rtl -> raise_s [%message "Invalid RTL specification" (rtl : string)])
  |> Command.Flag.optional_with_default Hardcaml.Rtl.Language.Verilog
;;

let parsexp s =
  match Parsexp.Single.parse_string s with
  | Ok s -> Ok s
  | Error e ->
    Or_error.error_s [%message "Failed to parse sexp" (e : Parsexp.Parse_error.t)]
;;

let yosys_netlist_of_json ~file_in =
  let%bind.Or_error json = Or_error.try_with (fun () -> In_channel.input_all file_in) in
  Expert.Yosys_netlist.of_string json
;;

let write_sexp ~file_out sexp_of_t t =
  let sexp = Sexp.to_string_hum (sexp_of_t t) in
  Out_channel.output_string file_out sexp
;;

let netlist_of_json ~file_in =
  let%bind.Or_error netlist = yosys_netlist_of_json ~file_in in
  Netlist.of_yosys_netlist netlist |> Or_error.ok_exn |> Netlist.get_all_modules
;;

let command_json_to_yosys_netlist =
  Command.basic
    ~summary:"Read a YOSYS JSON netlist and write as Yosys_netlist.t sexp"
    [%map_open.Command
      let file_in = flag "-i" in_chan ~doc:"JSON input file"
      and file_out = flag "-o" out_chan ~doc:"NETLIST output file" in
      fun () ->
        let yosys_netlist = yosys_netlist_of_json ~file_in in
        write_sexp ~file_out [%sexp_of: Expert.Yosys_netlist.t Or_error.t] yosys_netlist]
;;

let command_json_to_netlist =
  Command.basic
    ~summary:"Read a YOSYS JSON netlist and write as Netlist.t sexp"
    [%map_open.Command
      let file_in = flag "-i" in_chan ~doc:"JSON input file"
      and file_out = flag "-o" out_chan ~doc:"NETLIST output file" in
      fun () ->
        let yosys_netlist = netlist_of_json ~file_in in
        write_sexp ~file_out [%sexp_of: Netlist.Module.t list Or_error.t] yosys_netlist]
;;

let read_verilog_design ~file_in =
  let%bind.Or_error verilog_design = parsexp (In_channel.input_all file_in) in
  Or_error.try_with (fun () -> Verilog_design.t_of_sexp verilog_design)
;;

let synthesize_to_yosys_netlist ~file_in =
  let%bind.Or_error verilog_design = read_verilog_design ~file_in in
  let%bind.Or_error netlist = Expert.Synthesize.to_yosys_netlist verilog_design in
  Ok netlist
;;

let synthesize_to_netlist ~file_in =
  let%bind.Or_error verilog_design = read_verilog_design ~file_in in
  let%bind.Or_error netlist = Netlist.create verilog_design in
  Netlist.get_all_modules netlist
;;

let synthesize_to_circuit ~file_in =
  let%bind.Or_error verilog_design = read_verilog_design ~file_in in
  let%bind.Or_error netlist = Netlist.create verilog_design in
  let%bind.Or_error circuit =
    Verilog_circuit.create netlist ~top_name:(Verilog_design.top_name verilog_design)
  in
  let%bind.Or_error circuit = Verilog_circuit.to_hardcaml_circuit circuit in
  Ok circuit
;;

let write_rtl ~file_out ~rtl (circuit : Hardcaml.Circuit.t) =
  Hardcaml.Rtl.create rtl [ circuit ]
  |> Hardcaml.Rtl.top_levels_only
  |> Rope.to_string
  |> Out_channel.output_string file_out
;;

let command_synthesize_to_yosys_netlist =
  Command.basic
    ~summary:"Read a verilog design netlist and write as Yosys_netlist.t sexp"
    [%map_open.Command
      let file_in = flag "-i" in_chan ~doc:"VLOG_DESIGN input file"
      and file_out = flag "-o" out_chan ~doc:"NETLIST output file" in
      fun () ->
        let netlist = synthesize_to_yosys_netlist ~file_in in
        write_sexp ~file_out [%sexp_of: Expert.Yosys_netlist.t Or_error.t] netlist]
;;

let command_synthesize_to_netlist =
  Command.basic
    ~summary:"Read a verilog design and write as Netlist.t sexp"
    [%map_open.Command
      let file_in = flag "-i" in_chan ~doc:"VLOG_DESIGN input file"
      and file_out = flag "-o" out_chan ~doc:"NETLIST output file" in
      fun () ->
        let netlist = synthesize_to_netlist ~file_in in
        write_sexp ~file_out [%sexp_of: Netlist.Module.t list Or_error.t] netlist]
;;

let command_synthesize_to_verilog =
  Command.basic
    ~summary:"Read a verilog design and write as RTL file"
    [%map_open.Command
      let file_in = flag "-i" in_chan ~doc:"VLOG_DESIGN input file"
      and file_out = flag "-o" out_chan ~doc:"RTL output file"
      and rtl = flag "-rtl" rtl ~doc:"RTL write verilog (default) or vhdl" in
      fun () ->
        let circuit = synthesize_to_circuit ~file_in |> Or_error.ok_exn in
        write_rtl ~file_out ~rtl circuit]
;;

let command_synthesize_to_json =
  Command.basic
    ~summary:"Read a verilog design and convert to json"
    [%map_open.Command
      let file_in = flag "-i" in_chan ~doc:"VLOG_DESIGN input file"
      and json_file = flag "-o" (required string) ~doc:"JSON output file" in
      fun () ->
        let result =
          let%bind.Or_error verilog_design = read_verilog_design ~file_in in
          Expert.Synthesize.to_json_file ~verbose:true verilog_design ~json_file
        in
        Or_error.ok_exn result]
;;

let command_synthesize_to_ocaml_module =
  Command.basic
    ~summary:"Read a verilog design and convert to an OCaml module"
    [%map_open.Command
      let file_in = flag "-i" in_chan ~doc:"VLOG_DESIGN input file"
      and file_out = flag "-o" out_chan ~doc:"OCAML output file"
      and path = flag "-path" (optional string) ~doc:"PREFIX of path to verilog files" in
      fun () ->
        let ocaml =
          let%bind.Or_error verilog_design = read_verilog_design ~file_in in
          let verilog_design =
            match path with
            | None -> verilog_design
            | Some path ->
              Verilog_design.map_paths verilog_design ~f:(fun vlog_path ->
                Filename.concat path vlog_path)
          in
          let%bind.Or_error netlist = Netlist.create ~verbose:true verilog_design in
          let%bind.Or_error circuit =
            Verilog_circuit.create
              netlist
              ~top_name:(Verilog_design.top_name verilog_design)
          in
          let ocaml = Ocaml_module.to_ocaml verilog_design circuit in
          Ok ocaml
        in
        Out_channel.output_string file_out (Or_error.ok_exn ocaml)]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Convert YOSYS JSON netlists"
       [ ( "json"
         , Command.group
             ~summary:""
             [ "yosys-netlist", command_json_to_yosys_netlist
             ; "netlist", command_json_to_netlist
             ] )
       ; ( "synthesize"
         , Command.group
             ~summary:"Convert verilog designs"
             [ "yosys-netlist", command_synthesize_to_yosys_netlist
             ; "netlist", command_synthesize_to_netlist
             ; "verilog", command_synthesize_to_verilog
             ; "ocaml-module", command_synthesize_to_ocaml_module
             ; "json", command_synthesize_to_json
             ] )
       ])
;;
