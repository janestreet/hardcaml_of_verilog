open Hardcaml
open Signal
open Hardcaml_of_verilog
module Cs = Cyclesim
module S = Cyclesim
module Waveterm_waves = Hardcaml_waveterm.Waves
module Waveterm_sim = Hardcaml_waveterm.Sim
module Waveterm_widget = Hardcaml_waveterm_interactive.Widget

let clock = input "clock" 1

let testbench () =
  let module C = struct
    let abits = 4
    let dbits = 8
    let size = 1 lsl abits
  end
  in
  let module L = Lvt.Make_wren (C) in
  let nwr = 2 in
  let nrd = 1 in
  let layout = [| [| 0; 0; 1; 1; 2; 2; 3; 3 |]; [| 4; 4; 4; 4; 5; 5; 5; 5 |] |] in
  let mode = Array.init nrd (fun _ -> `sync_rbw) in
  let aname m n = n ^ string_of_int m in
  let mk_input m (n, b) = input (aname m n) b in
  let wr = Array.init nwr (fun m -> L.Wr.(map ~f:(mk_input m) t)) in
  let rd = Array.init nrd (fun m -> L.Rd.(map ~f:(mk_input m) t)) in
  let wr' =
    Array.init nwr (fun m ->
      { Lvt.ram_spec = Reg_spec.create () ~clock
      ; reg_spec = Reg_spec.create () ~clock
      ; wr = wr.(m)
      })
  in
  let rd' =
    Array.init nrd (fun m ->
      { Lvt.reg_spec = Reg_spec.create () ~clock; rd = rd.(m); mode = mode.(m) })
  in
  let q = L.memory ~layout ~wr:wr' ~rd:rd' in
  let q = Array.init nrd (fun m -> output (aname m "q") q.(m)) in
  let circ = Circuit.create_exn ~name:"wrram" (Array.to_list q) in
  let sim = Cs.create circ in
  let sim, waves = Waveterm_sim.wrap sim in
  let rd m =
    { L.Rd.ra = S.in_port sim (aname m "ra")
    ; re =
        (try S.in_port sim (aname m "re") with
         | _ -> ref Bits.vdd)
    }
  in
  let wr m =
    { L.Wr.wa = S.in_port sim (aname m "wa")
    ; we = S.in_port sim (aname m "we")
    ; d = S.in_port sim (aname m "d")
    }
  in
  let rd, wr = Array.init nrd rd, Array.init nwr wr in
  let open L.Wr in
  let open L.Rd in
  let cycle () =
    S.cycle sim;
    Array.iter (fun wr -> wr.we := Bits.of_int ~width:C.dbits 0) wr;
    Array.iter (fun rd -> rd.re := Bits.gnd) rd
  in
  wr.(0).wa := Bits.of_int ~width:C.abits 0;
  wr.(0).we := Bits.of_string "00000011";
  wr.(0).d := Bits.of_int ~width:C.dbits 255;
  wr.(1).wa := Bits.of_int ~width:C.abits 1;
  wr.(1).we := Bits.of_string "11110000";
  wr.(1).d := Bits.of_int ~width:C.dbits 255;
  cycle ();
  rd.(0).ra := Bits.of_int ~width:C.abits 0;
  rd.(0).re := Bits.vdd;
  cycle ();
  rd.(0).ra := Bits.of_int ~width:C.abits 1;
  rd.(0).re := Bits.vdd;
  cycle ();
  cycle ();
  cycle ();
  Waveterm_widget.run_and_close Waveterm_waves.{ cfg = Config.default; waves }
;;

let () = testbench ()
