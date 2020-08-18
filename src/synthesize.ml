open! Import
module List = Base.List

let tempfile ext = Filename.temp_file "hardcaml_of_verilog_synthesize_" ext

let unlink ok file =
  (* make true to keep temporary files for debugging *)
  let debug = false in
  if ok || not debug then Unix.unlink file
;;

let convert_to_json_file
      ~(params : Parameter.t list)
      ~topname
      ~blackboxes
      ~verilog
      ~json_file
  =
  let script_file = tempfile ".yosys" in
  let script = Out_channel.create script_file in
  (* verilog files loaded in order to extract their interfaces for use as blackboxes *)
  List.iter blackboxes ~f:(fprintf script "read_verilog -defer -lib %s\n");
  (* verilog files containing designs which will be synthesized into json *)
  List.iter verilog ~f:(fprintf script "read_verilog -defer %s\n");
  (* set top level parameters.
     workaround applied for yosys 0.6 https://github.com/cliffordwolf/yosys/issues/132 *)
  List.iter params ~f:(fun p ->
    fprintf
      script
      "chparam -set %s %s $abstract\\%s\n"
      (p.name |> Parameter_name.to_string)
      (match p.value with
       | Int i -> sprintf "%i" i
       | String s -> "\"" ^ s ^ "\""
       | _ -> raise_s [%message "Unsupported parameter type" (p : Parameter.t)])
      topname);
  (* standard synthesis passes - set top level design *)
  fprintf script "hierarchy -top %s\n" topname;
  (* convert always blocks into muxes *)
  fprintf script "proc\n";
  fprintf script "flatten\n";
  (* convert memories *)
  fprintf script "memory -nomap; opt; clean\n";
  (* clean up *)
  fprintf script "opt -mux_undef\n";
  fprintf script "clean\n";
  (* write to json *)
  fprintf script "write_json %s\n" json_file;
  Out_channel.close script;
  (* run yosys *)
  let stat = Unix.system @@ Run.with_script ~verbose:false () ~script_file in
  unlink (Result.is_ok stat) script_file;
  Unix.Exit_or_signal.or_error stat
;;

let convert_to_json_string ~params ~topname ~blackboxes ~verilog =
  let json_file = tempfile ".json" in
  let result =
    Or_error.try_with (fun () ->
      Or_error.ok_exn
        (convert_to_json_file ~params ~topname ~blackboxes ~verilog ~json_file);
      In_channel.read_all json_file)
  in
  unlink (Result.is_ok result) json_file;
  result
;;

let convert_to_json_netlist ~params ~topname ~blackboxes ~verilog =
  Or_error.map
    (convert_to_json_string ~params ~topname ~blackboxes ~verilog)
    ~f:Json_netlist.of_string
;;
