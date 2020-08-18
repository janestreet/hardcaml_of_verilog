module Default = struct
  let blackbox = false

  (* Files are in the location specified in the Verilog_design.t sexp. *)
  let file_io = None
end

let simple_adder_8 () =
  let module Inst = Simple_adder_8.From_verilog () (Default) in
  let module Circ = Hardcaml.Circuit.With_interface (Inst.I) (Inst.O) in
  Circ.create_exn ~name:Simple_adder_8.name Inst.create |> Hardcaml.Rtl.print Verilog
;;

let simple_adder_16 () =
  let module Inst = Simple_adder_16.From_verilog () (Default) in
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

let carry_save_adder blackbox =
  let module Inst =
    Carry_save_adder.From_verilog
      ()
      (struct
        let blackbox = blackbox

        (* Example custom read mode.  Just reads the file at the given path. *)
        let file_io =
          Some
            (Hardcaml_of_verilog.Verilog_design.File_io.Path_to_data
               Stdio.In_channel.read_all)
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
let () = carry_save_adder false
let () = carry_save_adder true
let () = carry_save_adder_json ()
