(*
   Multiport memories for Hardcaml
   ===============================

   The basic memory primitive provide by Hardcaml has one write (wr) and
   one read (rd) port.  The write is synchronous and the read is asynchronous.
   By adding a register to either the read address or output data we can
   generate standard read-before-write and write-before-read synchronous
   memories.  The read and write clocks can be from different domains.

   A simple way to build a multi-port memory is out of simple registers
   This is, of course, very inefficient.  One restriction that
   still exists is that each write port must be from the same clock
   domain.  I (think) the reads ports could potentially be in differing
   clock domains.

   LVT
   ===

   To do a bit better we can instead build multi-port memories using
   a Live Value Table (LVT).  The idea here is in 3 steps;

   1] To make N rd ports, we replicate a 1 rd ram N times each written
   with the same data.  Note each replicated RAM will contain exactly
   the same data.  The replication provides N access ports to this data.
   Call this memory_nrd.

   2] To make M wr ports we initially replicate memory_nrd M times.
   Each write port is connected to one memory_nrd instance.  Note that
   each memory_nrd bank will this time contain different data.
   We now have M * N ram outputs to select between to for the N read
   ports.

   3] Build the live value table.  This tracks the bank which was most
   recently written for each address in the RAM.  This is built as
   a multi-port memory itself, specifically using the simple register
   scheme described before.  The outputs of the LVT (one for each read
   port) selects the bank built in step 2 that contains the most
   recently written value for a particular address.

   Consider 2 read, 4 write ports on a 256 element x 32 bit memory.
   Using the pure register scheme we will need to generate 256x32
   register bits, write port selection logic at the input to each
   register, and 2 256x32 muxes to select the read data.

   With the LVT scheme we still need a register based multiport
   memory - in this case 2 read, 4 write ports on a 256 element x 2 bit
   memory.  Note the change here - we went from a 32 bit memory (to
   store the data) to a 2 bit memory (to store the index of the latest
   write bank ie it's of width log2(number of write ports).  In this
   case we have save approx 16x logic resources.

   Of course we also need 2*4 256 x 32 memories (with 1 read and 1 write
   port each) to store the data.
*)

open! Base
open Hardcaml
open Signal

module type Config = sig
  val abits : int
  val dbits : int
  val size : int
end

module Wr = struct
  type 'a t =
    { we : 'a
    ; wa : 'a
    ; d : 'a
    }
  [@@deriving hardcaml]
end

module Rd = struct
  type 'a t =
    { re : 'a
    ; ra : 'a
    }
  [@@deriving hardcaml]
end

(* fallthrough = wbr (write before read) *)
type mode =
  [ `async_rbw
  | `async_wbr
  | `sync_rbw
  | `sync_wbr
  ]

let is_sync = function
  | `sync_wbr | `sync_rbw -> true
  | _ -> false
;;

let is_async m = not (is_sync m)

let is_rbw = function
  | `sync_rbw | `async_rbw -> true
  | _ -> false
;;

let is_wbr m = not (is_rbw m)

type wr_port =
  { wr : t Wr.t
  ; ram_spec : Reg_spec.t
  ; reg_spec : Reg_spec.t
  }

type rd_port =
  { rd : t Rd.t
  ; reg_spec : Reg_spec.t
  ; mode : mode
  }

module Ports (C : Config) = struct
  module Wr = struct
    include Wr

    let bits = { we = 1; wa = C.abits; d = C.dbits }
    let port_names_and_widths = zip port_names bits
  end

  module Rd = struct
    include Rd

    let bits = { re = 1; ra = C.abits }
    let port_names_and_widths = zip port_names bits
  end
end

module Multiport_regs (C : Config) = struct
  (* async read memories with multiple read and write ports, implemented as registers *)

  open C
  include Ports (C)
  open Wr
  open Rd

  let pri =
    tree ~arity:2 ~f:(function
      | [ a ] -> a
      | [ (s0, d0); (s1, d1) ] -> s1 |: s0, mux2 s1 d1 d0
      | _ -> empty, empty)
  ;;

  let reg_we_enable ~we ~wa =
    (binary_to_onehot wa).:[size - 1, 0] &: mux2 we (ones size) (zero size)
  ;;

  let memory_nwr_array ~(wr : wr_port array) =
    let reg_spec = wr.(0).reg_spec in
    let wr = List.map ~f:(fun wr -> wr.wr) @@ Array.to_list wr in
    let we1h = List.map ~f:(fun wr -> reg_we_enable ~we:wr.we ~wa:wr.wa) wr in
    Array.to_list
    @@ Array.init size ~f:(fun elt ->
      let wed = List.map2_exn ~f:(fun we1h wr -> we1h.:(elt), wr.d) we1h wr in
      let we, d = pri wed in
      (* last d with write enable set *)
      let r = reg reg_spec ~enable:we d in
      we, d, r)
  ;;

  (* n write, n read ports *)
  let memory ~(wr : wr_port array) ~(rd : rd_port array) =
    let base = memory_nwr_array ~wr in
    Array.init (Array.length rd) ~f:(fun i ->
      let reg_spec = rd.(i).reg_spec in
      let mr = List.map ~f:(fun (we, d, r) -> mux2 we d r) base in
      let r = List.map ~f:(fun (_, _, r) -> r) base in
      match rd.(i).mode with
      | `async_wbr -> mux rd.(i).rd.ra mr
      | `async_rbw -> mux rd.(i).rd.ra r
      | `sync_wbr -> mux (reg reg_spec ~enable:rd.(i).rd.re rd.(i).rd.ra) r
      | `sync_rbw -> reg reg_spec ~enable:rd.(i).rd.re (mux rd.(i).rd.ra r))
  ;;
end

module Make (C : Config) = struct
  include Ports (C)
  open Wr
  open Rd

  (* compatibility shim *)
  let memory ram_spec size ~we ~wa ~d ~ra =
    memory
      size
      ~write_port:
        { write_clock = Reg_spec.clock ram_spec
        ; write_enable = we
        ; write_address = wa
        ; write_data = d
        }
      ~read_address:ra
  ;;

  let memory_1rd ~wr ~rd =
    let ram_spec = wr.ram_spec in
    let reg_spec = rd.reg_spec in
    let mode = rd.mode in
    let wr, rd = wr.wr, rd.rd in
    match mode with
    | `sync_rbw ->
      reg
        reg_spec
        ~enable:rd.re
        (memory ram_spec C.size ~we:wr.we ~wa:wr.wa ~d:wr.d ~ra:rd.ra)
    | `sync_wbr ->
      memory
        ram_spec
        C.size
        ~we:wr.we
        ~wa:wr.wa
        ~d:wr.d
        ~ra:(reg reg_spec ~enable:rd.re rd.ra)
    | `async_rbw -> memory ram_spec C.size ~we:wr.we ~wa:wr.wa ~d:wr.d ~ra:rd.ra
    | `async_wbr ->
      mux2
        (wr.we &: (wr.wa ==: rd.ra))
        wr.d
        (memory ram_spec C.size ~we:wr.we ~wa:wr.wa ~d:wr.d ~ra:rd.ra)
  ;;

  let memory_nrd ~wr ~rd =
    let nrd = Array.length rd in
    Array.init nrd ~f:(fun i -> memory_1rd ~wr ~rd:rd.(i))
  ;;

  let memory ~wr ~rd =
    let nwr, nrd = Array.length wr, Array.length rd in
    (* create the live value table *)
    let module Lvt_cfg = struct
      let abits = C.abits
      let dbits = Int.ceil_log2 nwr
      let size = C.size
    end
    in
    let module Lvt = Multiport_regs (Lvt_cfg) in
    let lvt =
      if nwr = 1
      then [||]
      else (
        let lvt_wr =
          Array.init nwr ~f:(fun i ->
            { (wr.(i)) with
              wr = { wr.(i).wr with d = of_int_trunc ~width:Lvt_cfg.dbits i }
            })
        in
        Lvt.memory ~wr:lvt_wr ~rd)
    in
    (* create the memory banks *)
    let mem = Array.init nwr ~f:(fun i -> memory_nrd ~wr:wr.(i) ~rd) in
    (* select the correct memory bank *)
    Array.init nrd ~f:(fun rd ->
      let mem = Array.init nwr ~f:(fun wr -> mem.(wr).(rd)) in
      if nwr = 1 then mem.(0) else mux lvt.(rd) (Array.to_list mem))
  ;;
end

module Make_wren (C : Config) = struct
  module Wr = struct
    include Wr

    let bits = { we = C.dbits; wa = C.abits; d = C.dbits }
    let port_names_and_widths = zip port_names bits
  end

  module Rd = struct
    include Rd

    let bits = { re = 1; ra = C.abits }
    let port_names_and_widths = zip port_names bits
  end

  module L = Make (C)

  let get_layout wrnets =
    let runs list =
      let rec f acc prev l =
        match prev, l with
        | None, [] -> []
        | None, h :: t -> f acc (Some (h, 1)) t
        | Some (prev, run), [] -> List.rev ((prev, run) :: acc)
        | Some (prev, run), h :: t ->
          if Poly.equal prev h
          then f acc (Some (prev, run + 1)) t
          else f ((prev, run) :: acc) (Some (h, 1)) t
      in
      f [] None list
    in
    let transpose a =
      let i0 = Array.length a in
      let i1 = Array.length a.(0) in
      Array.init i1 ~f:(fun i1 -> Array.init i0 ~f:(fun i0 -> a.(i0).(i1)))
    in
    let rec starts pos = function
      | [] -> []
      | (h, r) :: t -> (h, pos, r) :: starts (pos + r) t
    in
    starts 0 @@ runs @@ Array.to_list @@ transpose wrnets
  ;;

  let memory ~layout =
    let layout = get_layout layout in
    let memory ~wr ~rd =
      let nrd = Array.length rd in
      let concat l =
        Array.init nrd ~f:(fun i -> concat_lsb @@ List.map ~f:(fun x -> x.(i)) l)
      in
      concat
      @@ List.map
           ~f:(fun (_, n, bits) ->
             let sel_wr wr =
               { wr with
                 wr =
                   { wr.wr with
                     Wr.we = wr.wr.Wr.we.:[n, n]
                   ; d = wr.wr.Wr.d.:[n + bits - 1, n]
                   }
               }
             in
             let wr = Array.map ~f:sel_wr wr in
             L.memory ~wr ~rd)
           layout
    in
    memory
  ;;
end
