open Base

module Default = struct
  let verbose = false
  let map_verilog_design = Fn.id
end

let simple_adder_8 () =
  let module Inst = Simple_adder_8.From_verilog (struct end) (Default) in
  let module Circ = Hardcaml.Circuit.With_interface (Inst.I) (Inst.O) in
  Circ.create_exn ~name:Simple_adder_8.name Inst.create |> Hardcaml.Rtl.print Verilog
;;

let simple_adder_16 () =
  let module Inst = Simple_adder_16.From_verilog (struct end) (Default) in
  let module Circ = Hardcaml.Circuit.With_interface (Inst.I) (Inst.O) in
  Circ.create_exn ~name:Simple_adder_16.name Inst.create |> Hardcaml.Rtl.print Verilog
;;

let simple_adder () =
  let module Inst =
    Simple_adder.From_verilog
      (struct
        let n = 23
      end)
      (Default)
  in
  let module Circ = Hardcaml.Circuit.With_interface (Inst.I) (Inst.O) in
  Circ.create_exn ~name:Simple_adder.name Inst.create |> Hardcaml.Rtl.print Verilog
;;

let carry_save_adder () =
  let module Inst =
    Carry_save_adder.From_verilog (struct end)
      (struct
        let verbose = false

        (* Example custom read mode. Copies the file to tmp, and modifies the
           verilog_design. Note we must take care to share files if there are multiple
           verilog modules in the same file. *)
        let map_verilog_design v =
          let seen = Hashtbl.create (module String) in
          Hardcaml_of_verilog.Verilog_design.map_paths v ~f:(fun path ->
            match Hashtbl.find seen path with
            | None ->
              let tmp_file = Filename_unix.temp_file "tmp" ".v" in
              Stdio.Out_channel.write_all tmp_file ~data:(Stdio.In_channel.read_all path);
              Hashtbl.set seen ~key:path ~data:tmp_file;
              tmp_file
            | Some tmp_file -> tmp_file)
        ;;
      end)
  in
  let module Circ = Hardcaml.Circuit.With_interface (Inst.I) (Inst.O) in
  Circ.create_exn ~name:Carry_save_adder.name Inst.create |> Hardcaml.Rtl.print Verilog
;;

(* Read data from a pregenerated json file (see jbuild). *)
let carry_save_adder_json () =
  let module Inst =
    Carry_save_adder.From_json (struct
      let json = Stdio.In_channel.read_all "carry_save_adder.json"
    end)
  in
  let module Circ = Hardcaml.Circuit.With_interface (Inst.I) (Inst.O) in
  Circ.create_exn ~name:Carry_save_adder.name Inst.create |> Hardcaml.Rtl.print Verilog
;;

let () = simple_adder_8 ()
let () = simple_adder_16 ()
let () = simple_adder ()
let () = carry_save_adder ()
let () = carry_save_adder ()
let () = carry_save_adder_json ()
