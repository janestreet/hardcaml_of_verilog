open Hardcaml
open Signal
open Hardcaml_of_verilog
module Cs = Cyclesim
module S = Cyclesim
module Waveterm_waves = Hardcaml_waveterm.Waves
module Waveterm_sim = Hardcaml_waveterm.Sim
module Waveterm_widget = Hardcaml_waveterm_interactive.Widget

(* configuration *)

let n_cycles = ref 1000
let abits = ref 4
let dbits = ref 8
let nwr = ref 2
let nrd = ref 2
let random_re = ref true
let mode : [ `alternate | Lvt.mode ] ref = ref `alternate
let vlog = ref false
let no_waves = ref false

let _ =
  Arg.(
    parse
      [ "-n", Set_int n_cycles, "Number of cycles to simulate (default: 1000)"
      ; "-a", Set_int abits, "address bits (default: 4)"
      ; "-d", Set_int dbits, "data bits (default: 8)"
      ; "-rd", Set_int nrd, "read ports (default: 2)"
      ; "-wr", Set_int nwr, "write ports (default: 2)"
      ; "-no-re", Clear random_re, "disable random read-enable toggling (default: false)"
      ; "-sync-rbw", Unit (fun () -> mode := `sync_rbw), "Put all ports in sync_rbw mode"
      ; "-sync-wbr", Unit (fun () -> mode := `sync_wbr), "Put all ports in sync_wbr mode"
      ; ( "-async-rbw"
        , Unit (fun () -> mode := `async_rbw)
        , "Put all ports in async_rbw mode" )
      ; ( "-async-wbr"
        , Unit (fun () -> mode := `async_wbr)
        , "Put all ports in async_wbr mode" )
      ; "-vlog", Set vlog, "Dump verilog"
      ; "-no-waves", Set no_waves, "Disable waveform view"
      ]
      (fun _ -> failwith "invalid anon arg")
      "LVT Multiport Memory Testbench.Builds memories with N read/M write ports from \
       simpler 1 read/1 write port memories (as available in FPGAs).  Requires N*M base \
       memories plus a so called Live Value Table to directs reads to the mostrecently \
       accessed write data.Each port may be set independently in syncronous or \
       asynchronous read mode withread-before-write or write-before-read behaviour (by \
       default the testbench alternates between port modes).For testing the read-enable \
       port may be held constant.")
;;

let clock = input "clock" 1

let testbench_mram () =
  let module C = struct
    let abits = !abits
    let dbits = !dbits
    let size = 1 lsl abits
  end
  in
  let nwr = !nwr in
  let nrd = !nrd in
  let mode =
    Array.init nrd (fun i ->
      match !mode with
      | `alternate ->
        (match i mod 4 with
         | 0 -> `sync_rbw
         | 1 -> `sync_wbr
         | 2 -> `async_rbw
         | _ -> `async_wbr)
      | `sync_rbw -> `sync_rbw
      | `sync_wbr -> `sync_wbr
      | `async_rbw -> `async_rbw
      | `async_wbr -> `async_wbr)
  in
  let random_re = !random_re in
  (*let module M = Memory_regs(C) in*)
  let module L = Lvt.Make (C) in
  let aname m n = n ^ string_of_int m in
  let mk_input m (n, b) = input (aname m n) b in
  let wr = Array.init nwr (fun m -> L.Wr.(map ~f:(mk_input m) t)) in
  let rd = Array.init nrd (fun m -> L.Rd.(map ~f:(mk_input m) t)) in
  let reg_spec = Reg_spec.create () ~clock in
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
  let q = L.memory ~wr:wr' ~rd:rd' in
  let qi = Array.init nrd (fun m -> input (aname m "qi") C.dbits) in
  let reg_qr m =
    match mode.(m) with
    | `sync_wbr -> reg reg_spec ~enable:vdd qi.(m)
    | `sync_rbw -> reg reg_spec ~enable:rd.(m).L.Rd.re qi.(m)
    | _ -> qi.(m)
  in
  let qr = Array.init nrd reg_qr in
  let qs =
    Array.init nrd (fun m -> output (aname m "q") q.(m), output (aname m "qr") qr.(m))
  in
  let check = Array.map (fun (q, qr) -> q ==: qr) qs in
  let check = Array.mapi (fun m -> output (aname m "check")) check in
  let outputs =
    (Array.to_list @@ Array.map fst qs)
    @ (Array.to_list @@ Array.map snd qs)
    @ Array.to_list check
  in
  let circ = Circuit.create_exn ~name:"mram" outputs in
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
  let qi = Array.init nrd (fun m -> S.in_port sim (aname m "qi")) in
  (* random reads and writes *)
  let rand (_, b) = b, Random.int (1 lsl b) in
  let rand_wr () = L.Wr.(map ~f:rand t) in
  let rand_rd () =
    if random_re then L.Rd.(map ~f:rand t) else L.Rd.{ (map ~f:rand t) with re = 1, 1 }
  in
  (* implement the reads and writes *)
  let ref_mem = Array.init C.size (fun _ -> 0) in
  (* do random reads and writes to the core and a testbench memory *)
  let assign a (b, c) = a := Bits.of_int ~width:b c in
  let update_write wr =
    let wr = L.Wr.(map ~f:snd wr) in
    if wr.L.Wr.we = 1 then ref_mem.(wr.L.Wr.wa) <- wr.L.Wr.d
  in
  let prev_ra = Array.make nrd 0 in
  let update_read i rd =
    (*let rd = L.Rd.({ map snd rd with L.Rd.re = 1 }) in*)
    let rd = L.Rd.(map ~f:snd rd) in
    if rd.L.Rd.re = 1 || Lvt.is_async mode.(i)
    then (
      qi.(i) := Bits.of_int ~width:C.dbits ref_mem.(rd.L.Rd.ra);
      prev_ra.(i) <- rd.L.Rd.ra)
    else if mode.(i) = `sync_wbr
    then qi.(i) := Bits.of_int ~width:C.dbits ref_mem.(prev_ra.(i))
  in
  let perform_reads ft =
    let cond = function
      | `sync_wbr | `async_wbr -> ft
      | _ -> not ft
    in
    for i = 0 to nrd - 1 do
      if cond mode.(i)
      then (
        let rd' = rand_rd () in
        update_read i rd';
        ignore @@ L.Rd.(map2 ~f:assign rd.(i) rd'))
    done
  in
  let perform_writes () =
    for i = 0 to nwr - 1 do
      let wr' = rand_wr () in
      update_write wr';
      ignore @@ L.Wr.(map2 ~f:assign wr.(i) wr')
    done
  in
  for _ = 1 to !n_cycles do
    perform_reads false;
    (* non-fallthrough reads *)
    perform_writes ();
    (* all writes (last takes priority) *)
    perform_reads true;
    (* fallthrough reads *)
    S.cycle sim
  done;
  if not !no_waves then Waveterm_widget.run Waveterm_waves.{ cfg = Config.default; waves };
  if !vlog then Rtl.print Verilog circ
;;

let () = testbench_mram ()
