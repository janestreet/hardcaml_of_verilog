(* *)

let picorv32 () =
  let module Inst =
    Picorv32.From_verilog
      (Picorv32.P)
      (struct
        let blackbox = false
        let file_io = None
      end)
  in
  let module Circ = Hardcaml.Circuit.With_interface (Inst.I) (Inst.O) in
  Circ.create_exn ~name:Picorv32.name Inst.create |> Hardcaml.Rtl.print Verilog
;;

let () = picorv32 ()
