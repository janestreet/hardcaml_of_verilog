open! Import
open Hardcaml

exception Invalid_parameter of string
exception Invalid_input of string

module Cell_implementation = struct
  type t = Parameter.t list -> (string * Signal.t) list -> (string * Signal.t) list
end

type t =
  { blackboxes : string list
  ; cell_fns : (string * (Cell_reference.t -> Cell_implementation.t)) list
  }

let pint = function
  | { Parameter.name = _; value = Int i } -> i
  | _ -> raise (Invalid_parameter "expecting int parameter")
;;

let pstr = function
  | { Parameter.name = _; value = String s } -> s
  | _ -> raise (Invalid_parameter "expecting string parameter")
;;

let pconst w = function
  | { Parameter.name = _; value = Int i } -> Signal.of_int ~width:w i
  | { Parameter.name = _; value = String s } ->
    Signal.of_string (Int.to_string w ^ "'b" ^ s)
  | _ -> raise (Invalid_parameter "bad const value")
;;

module Simlib = struct
  open Signal

  let ( ^~: ) a b = ~:(a ^: b)

  module Wrapper
      (P : Hardcaml.Interface.S)
      (I : Hardcaml.Interface.S)
      (O : Hardcaml.Interface.S) : sig
    type fn =
      string * (Cell_reference.t -> Parameter.t P.t -> Signal.t I.t -> Signal.t O.t)

    val wrapper : fn -> string * (Cell_reference.t -> Cell_implementation.t)
  end = struct
    type fn =
      string * (Cell_reference.t -> Parameter.t P.t -> Signal.t I.t -> Signal.t O.t)

    let of_list_p l =
      P.map
        ~f:(fun (n, _) ->
          let name = Parameter_name.of_string n in
          { Parameter.name; value = Parameter.find_name_exn l name })
        P.t
    ;;

    let of_list_i l =
      I.map ~f:(fun (n, _) -> List.Assoc.find_exn l n ~equal:String.equal) I.t
    ;;

    let to_list_o o = O.to_list @@ O.map2 ~f:(fun (n, _) x -> n, x) O.t o
    let wrapper (n, f) = n, fun c p i -> to_list_o @@ f c (of_list_p p) (of_list_i i)
  end

  module Op1 = struct
    module P = struct
      type 'a t =
        { a_signed : 'a [@rtlname "A_SIGNED"]
        ; a_width : 'a [@rtlname "A_WIDTH"]
        ; y_width : 'a [@rtlname "Y_WIDTH"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t = { a : 'a [@rtlname "A"] } [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { y : 'a [@rtlname "Y"] } [@@deriving sexp_of, hardcaml]
    end

    module W = Wrapper (P) (I) (O)

    let res p = if p.P.a_signed = 1 then sresize else uresize

    let f1 f _ p i =
      let p = P.map ~f:pint p in
      assert (width i.I.a = p.P.a_width);
      let a = (res p) i.I.a p.P.y_width in
      O.{ y = f a }
    ;;

    let not_ = "$not", f1 ( ~: )
    let pos = "$pos", f1 (fun x -> x)
    let neg = "$neg", f1 negate

    let fr f _ p i =
      let p = P.map ~f:pint p in
      assert (width i.I.a = p.P.a_width);
      O.{ y = uresize (reduce ~f (bits_msb i.I.a)) p.P.y_width }
    ;;

    let reduce_or = "$reduce_or", fr ( |: )
    let reduce_and = "$reduce_and", fr ( &: )
    let reduce_xor = "$reduce_xor", fr ( ^: )

    let reduce_xnor =
      ( "$reduce_xnor"
      , fun _ p i ->
        let p = P.map ~f:pint p in
        assert (width i.I.a = p.P.a_width);
        let y = reduce ~f:( ^: ) (bits_msb i.I.a) in
        O.{ y = uresize ~:y p.P.y_width } )
    ;;

    let reduce_bool = "$reduce_bool", fr ( |: )

    let logic_not =
      ( "$logic_not"
      , fun _ p i ->
        let p = P.map ~f:pint p in
        assert (width i.I.a = p.P.a_width);
        let y = i.I.a ==:. 0 in
        O.{ y = uresize y p.P.y_width } )
    ;;

    let cells =
      [ not_
      ; pos
      ; neg
      ; reduce_or
      ; reduce_and
      ; reduce_xor
      ; reduce_xnor
      ; reduce_bool
      ; logic_not
      ]
    ;;

    let get_input_width p = I.{ a = p.P.a_width }
    let get_output_width p = O.{ y = p.P.y_width }
  end

  module Op2 = struct
    module P = struct
      type 'a t =
        { a_signed : 'a [@rtlname "A_SIGNED"]
        ; b_signed : 'a [@rtlname "B_SIGNED"]
        ; a_width : 'a [@rtlname "A_WIDTH"]
        ; b_width : 'a [@rtlname "B_WIDTH"]
        ; y_width : 'a [@rtlname "Y_WIDTH"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t =
        { a : 'a [@rtlname "A"]
        ; b : 'a [@rtlname "B"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { y : 'a [@rtlname "Y"] } [@@deriving sexp_of, hardcaml]
    end

    module W = Wrapper (P) (I) (O)

    let res p = if p.P.a_signed = 1 && p.P.b_signed = 1 then sresize else uresize

    let f2 f _ p i =
      let p = P.map ~f:pint p in
      assert (width i.I.a = p.P.a_width);
      assert (width i.I.b = p.P.b_width);
      let a = (res p) i.I.a p.P.y_width in
      let b = (res p) i.I.b p.P.y_width in
      O.{ y = uresize (f a b) p.P.y_width }
    ;;

    let and_ = "$and", f2 ( &: )
    let or_ = "$or", f2 ( |: )
    let xor_ = "$xor", f2 ( ^: )
    let xnor_ = "$xnor", f2 ( ^~: )
    let add = "$add", f2 ( +: )
    let sub = "$sub", f2 ( -: )

    let mul =
      ( "$mul"
      , fun _ p i ->
        let p = P.map ~f:pint p in
        assert (width i.I.a = p.P.a_width);
        assert (width i.I.b = p.P.b_width);
        let is_signed = p.P.a_signed = 1 && p.P.b_signed = 1 in
        let a = (res p) i.I.a p.P.y_width in
        let b = (res p) i.I.b p.P.y_width in
        let ( *: ) a b =
          if is_signed
          then sresize (a *+ b) p.P.y_width
          else uresize (a *: b) p.P.y_width
        in
        O.{ y = a *: b } )
    ;;

    let fs f _ p i =
      let p = P.map ~f:pint p in
      assert (width i.I.a = p.P.a_width);
      assert (width i.I.b = p.P.b_width);
      let a =
        (if p.P.a_signed = 1 then sresize else uresize)
          i.I.a
          (max p.P.y_width p.P.a_width)
      in
      O.{ y = uresize (f p.P.a_signed a i.I.b) p.P.y_width }
    ;;

    let shl = "$shl", fs (fun _ a b -> log_shift sll a b)
    let shr = "$shr", fs (fun _ a b -> log_shift srl a b)

    let fss f _ p i =
      let p = P.map ~f:pint p in
      assert (width i.I.a = p.P.a_width);
      assert (width i.I.b = p.P.b_width);
      let a =
        (if p.P.a_signed = 1 then sresize else uresize)
          i.I.a
          (max p.P.a_width p.P.y_width)
      in
      O.{ y = uresize (f p.P.a_signed a i.I.b) p.P.y_width }
    ;;

    let sshl = "$sshl", fss (fun _ a b -> log_shift sll a b)
    let sshr = "$sshr", fss (fun s a b -> log_shift (if s = 1 then sra else srl) a b)

    let shift =
      ( "$shift"
      , fun _ p i ->
        let p = P.map ~f:pint p in
        assert (width i.I.a = p.P.a_width);
        assert (width i.I.b = p.P.b_width);
        let a = uresize i.I.a (max p.P.a_width p.P.y_width) in
        let y =
          if p.P.b_signed = 1
          then mux2 (msb i.I.b) (log_shift sll a (negate i.I.b)) (log_shift srl a i.I.b)
          else log_shift srl a i.I.b
        in
        O.{ y = uresize y p.P.y_width } )
    ;;

    let shiftx =
      ( "$shiftx"
        ,
        fun _ p i ->
          let p = P.map ~f:pint p in
          assert (width i.I.a = p.P.a_width);
          assert (width i.I.b = p.P.b_width);
          let a = uresize i.I.a (max p.P.a_width p.P.y_width) in
          let y =
            if p.P.b_signed = 1
            then mux2 (msb i.I.b) (log_shift sll a (negate i.I.b)) (log_shift srl a i.I.b)
            else log_shift srl a i.I.b
          in
          O.{ y = uresize y p.P.y_width } )
    ;;

    (* let macc = ... *)
    (* let div = ... *)
    (* let mod = ... *)
    (* let pow = ... *)

    let fl f _ p i =
      let p = P.map ~f:pint p in
      assert (width i.I.a = p.P.a_width);
      assert (width i.I.b = p.P.b_width);
      O.{ y = uresize (f i.I.a i.I.b) p.P.y_width }
    ;;

    let logic_and = "$logic_and", fl (fun a b -> a <>:. 0 &: (b <>:. 0))
    let logic_or = "$logic_or", fl (fun a b -> a <>:. 0 |: (b <>:. 0))

    let fc fs fu _ p i =
      let p = P.map ~f:pint p in
      assert (width i.I.a = p.P.a_width);
      assert (width i.I.b = p.P.b_width);
      let w = max p.P.a_width p.P.b_width in
      let a = (res p) i.I.a w in
      let b = (res p) i.I.b w in
      O.
        { y =
            uresize
              ((if p.P.a_signed = 1 && p.P.b_signed = 1 then fs else fu) a b)
              p.P.y_width
        }
    ;;

    let lt = "$lt", fc ( <+ ) ( <: )
    let le = "$le", fc ( <=+ ) ( <=: )
    let gt = "$gt", fc ( >+ ) ( >: )
    let ge = "$ge", fc ( >=+ ) ( >=: )
    let eq = "$eq", fc ( ==: ) ( ==: )
    let ne = "$ne", fc ( <>: ) ( <>: )
    let eqx = "$eqx", fc ( ==: ) ( ==: )
    let nex = "$nex", fc ( <>: ) ( <>: )

    let cells =
      [ and_
      ; or_
      ; xor_
      ; xnor_
      ; add
      ; sub
      ; mul
      ; shl
      ; shr
      ; sshl
      ; sshr
      ; shift
      ; shiftx
      ; logic_and
      ; logic_or
      ; lt
      ; le
      ; gt
      ; ge
      ; eq
      ; ne
      ; eqx
      ; nex
      ]
    ;;

    let get_input_width p = I.{ a = p.P.a_width; b = p.P.b_width }
    let get_output_width p = O.{ y = p.P.y_width }
  end

  module Fa = struct
    module P = struct
      type 'a t = { width : 'a [@rtlname "WIDTH"] } [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t =
        { a : 'a [@rtlname "A"]
        ; b : 'a [@rtlname "B"]
        ; c : 'a [@rtlname "C"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { x : 'a [@rtlname "X"]
        ; y : 'a [@rtlname "Y"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module W = Wrapper (P) (I) (O)

    let fa _ p i =
      let wid = pint p.P.width in
      assert (width i.I.a = wid);
      assert (width i.I.b = wid);
      assert (width i.I.c = wid);
      let t1 = i.I.a ^: i.I.b in
      let t2 = i.I.a &: i.I.b in
      let t3 = i.I.c &: t1 in
      O.{ x = t2 |: t3; y = t1 ^: i.I.c }
    ;;

    let fa = "$fa", fa
    let cells = [ fa ]
    let get_input_width p = I.{ a = p.P.width; b = p.P.width; c = p.P.width }
    let get_output_width p = O.{ x = p.P.width; y = p.P.width }
  end

  module Lcu = struct
    module P = struct
      type 'a t = { width : 'a [@rtlname "WIDTH"] } [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t =
        { p : 'a [@rtlname "P"]
        ; g : 'a [@rtlname "G"]
        ; ci : 'a [@rtlname "CI"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { co : 'a [@rtlname "CO"] } [@@deriving sexp_of, hardcaml]
    end

    module W = Wrapper (P) (I) (O)

    let lcu _ p i =
      let wid = pint p.P.width in
      assert (width i.I.p = wid);
      assert (width i.I.g = wid);
      assert (width i.I.ci = 1);
      let p = bits_lsb i.I.p in
      let g = bits_lsb i.I.g in
      let rec f p g ci =
        match p, g with
        | [], [] -> []
        | p :: p', g :: g' ->
          let co = g |: (p &: ci) in
          co :: f p' g' co
        | _ -> failwith "'p' and 'g' list lengths in lcu are not the same"
      in
      let co = concat_lsb (f p g i.I.ci) in
      O.{ co }
    ;;

    let lcu = "$lcu", lcu
    let cells = [ lcu ]
    let get_input_width p = I.{ p = p.P.width; g = p.P.width; ci = 1 }
    let get_output_width p = O.{ co = p.P.width }
  end

  module Slice = struct
    module P = struct
      type 'a t =
        { offset : 'a [@rtlname "OFFSET"]
        ; a_width : 'a [@rtlname "A_WIDTH"]
        ; y_width : 'a [@rtlname "Y_WIDTH"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t = { a : 'a [@rtlname "A"] } [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { y : 'a [@rtlname "Y"] } [@@deriving sexp_of, hardcaml]
    end

    module W = Wrapper (P) (I) (O)

    let slice _ p i =
      let p = P.map ~f:pint p in
      assert (width i.I.a = p.P.a_width);
      O.{ y = uresize (srl i.I.a p.P.offset) p.P.y_width }
    ;;

    let slice = "$slice", slice
    let cells = [ slice ]
    let get_input_width p = I.{ a = p.P.a_width }
    let get_output_width p = O.{ y = p.P.y_width }
  end

  module Concat = struct
    module P = struct
      type 'a t =
        { a_width : 'a [@rtlname "A_WIDTH"]
        ; b_width : 'a [@rtlname "B_WIDTH"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t =
        { a : 'a [@rtlname "A"]
        ; b : 'a [@rtlname "B"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { y : 'a [@rtlname "Y"] } [@@deriving sexp_of, hardcaml]
    end

    module W = Wrapper (P) (I) (O)

    let concat _ p i =
      let p = P.map ~f:pint p in
      assert (width i.I.a = p.P.a_width);
      assert (width i.I.b = p.P.b_width);
      O.{ y = i.I.b @: i.I.a }
    ;;

    let concat = "$concat", concat
    let cells = [ concat ]
    let get_input_width p = I.{ a = p.P.a_width; b = p.P.b_width }
    let get_output_width p = O.{ y = p.P.a_width + p.P.b_width }
  end

  module Mux = struct
    module P = struct
      type 'a t = { width : 'a [@rtlname "WIDTH"] } [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t =
        { a : 'a [@rtlname "A"]
        ; b : 'a [@rtlname "B"]
        ; s : 'a [@rtlname "S"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { y : 'a [@rtlname "Y"] } [@@deriving sexp_of, hardcaml]
    end

    module W = Wrapper (P) (I) (O)

    let mux _ p i =
      let p = P.map ~f:pint p in
      assert (width i.I.a = p.P.width);
      assert (width i.I.b = p.P.width);
      assert (width i.I.s = 1);
      O.{ y = mux2 i.I.s i.I.b i.I.a }
    ;;

    let mux = "$mux", mux
    let cells = [ mux ]
    let get_input_width p = I.{ a = p.P.width; b = p.P.width; s = 1 }
    let get_output_width p = O.{ y = p.P.width }
  end

  module Pmux = struct
    module P = struct
      type 'a t =
        { width : 'a [@rtlname "WIDTH"]
        ; s_width : 'a [@rtlname "S_WIDTH"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t =
        { a : 'a [@rtlname "A"]
        ; b : 'a [@rtlname "B"]
        ; s : 'a [@rtlname "S"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { y : 'a [@rtlname "Y"] } [@@deriving sexp_of, hardcaml]
    end

    module W = Wrapper (P) (I) (O)

    let pmux _ p i =
      let p = P.map ~f:pint p in
      assert (width i.I.a = p.P.width);
      assert (width i.I.b = p.P.width * p.P.s_width);
      assert (width i.I.s = p.P.s_width);
      let rec pmux s a b i =
        match s with
        | [] -> a
        | s :: t ->
          let b' = select b (((i + 1) * p.P.width) - 1) (i * p.P.width) in
          mux2 s b' (pmux t a b (i + 1))
      in
      O.{ y = pmux (bits_lsb i.I.s) i.I.a i.I.b 0 }
    ;;

    let pmux = "$pmux", pmux
    let cells = [ pmux ]

    let get_input_width p =
      I.{ a = p.P.width; b = p.P.width * p.P.s_width; s = p.P.s_width }
    ;;

    let get_output_width p = O.{ y = p.P.width }
  end

  module Lut = struct
    module P = struct
      type 'a t =
        { width : 'a [@rtlname "WIDTH"]
        ; lut : 'a [@rtlname "LUT"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t = { a : 'a [@rtlname "A"] } [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { y : 'a [@rtlname "Y"] } [@@deriving sexp_of, hardcaml]
    end

    module W = Wrapper (P) (I) (O)

    let lut _ p i =
      let p = P.map ~f:pint p in
      assert (width i.I.a = p.P.width);
      let lut = of_int ~width:(1 lsl p.P.width) p.P.lut in
      let y = mux i.I.a (Array.to_list @@ Array.init (1 lsl p.P.width) ~f:(bit lut)) in
      O.{ y }
    ;;

    let lut = "$lut", lut
    let cells = [ lut ]
    let get_input_width p = I.{ a = p.P.width }
    let get_output_width _ = O.{ y = 1 }
  end

  (* module Alu = struct .. end *)
  (* module Tribuf = struct .. end *)
  (* module Assert = struct .. end *)
  (* module Assume = struct .. end *)
  (* module Equiv = struct .. end *)

  let clock = Hardcaml.Signal.input "clock" 1
  let reg_spec = Reg_spec.create () ~clock

  (* module Sr = struct end *)

  module Dff = struct
    module P = struct
      type 'a t =
        { width : 'a [@rtlname "WIDTH"]
        ; clk_polarity : 'a [@rtlname "CLK_POLARITY"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t =
        { clk : 'a [@rtlname "CLK"]
        ; d : 'a [@rtlname "D"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { q : 'a [@rtlname "Q"] } [@@deriving sexp_of, hardcaml]
    end

    module W = Wrapper (P) (I) (O)

    let dff _ p i =
      let open I in
      let p = P.map ~f:pint p in
      assert (width i.d = p.P.width);
      let clock_edge : Edge.t = if p.P.clk_polarity = 1 then Rising else Falling in
      O.
        { q = reg (Reg_spec.override reg_spec ~clock:i.clk ~clock_edge) ~enable:empty i.d
        }
    ;;

    let dff = "$dff", dff
    let cells = [ dff ]
    let get_input_width p = I.{ clk = 1; d = p.P.width }
    let get_output_width p = O.{ q = p.P.width }
  end

  module Dffe = struct
    module P = struct
      type 'a t =
        { width : 'a [@rtlname "WIDTH"]
        ; clk_polarity : 'a [@rtlname "CLK_POLARITY"]
        ; en_polarity : 'a [@rtlname "EN_POLARITY"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t =
        { clk : 'a [@rtlname "CLK"]
        ; en : 'a [@rtlname "EN"]
        ; d : 'a [@rtlname "D"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { q : 'a [@rtlname "Q"] } [@@deriving sexp_of, hardcaml]
    end

    module W = Wrapper (P) (I) (O)

    let dffe _ p i =
      let open I in
      let p = P.map ~f:pint p in
      assert (width i.d = p.P.width);
      let clock_edge : Edge.t = if p.P.clk_polarity = 1 then Rising else Falling in
      let enable = if p.P.en_polarity = 1 then i.en else ~:(i.en) in
      O.{ q = reg (Reg_spec.override reg_spec ~clock:i.clk ~clock_edge) ~enable i.d }
    ;;

    let dffe = "$dffe", dffe
    let cells = [ dffe ]
    let get_input_width p = I.{ clk = 1; en = 1; d = p.P.width }
    let get_output_width p = O.{ q = p.P.width }
  end

  module Dffsr = struct
    module P = struct
      type 'a t =
        { width : 'a [@rtlname "WIDTH"]
        ; clk_polarity : 'a [@rtlname "CLK_POLARITY"]
        ; set_polarity : 'a [@rtlname "SET_POLARITY"]
        ; clr_polarity : 'a [@rtlname "CLR_POLARITY"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t =
        { clk : 'a [@rtlname "CLK"]
        ; set : 'a [@rtlname "SET"]
        ; clr : 'a [@rtlname "CLR"]
        ; d : 'a [@rtlname "D"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { q : 'a [@rtlname "Q"] } [@@deriving sexp_of, hardcaml]
    end

    module W = Wrapper (P) (I) (O)

    let dffsr _ p i =
      let open I in
      let p = P.map ~f:pint p in
      assert (width i.d = p.P.width);
      assert (width i.set = p.P.width);
      assert (width i.clr = p.P.width);
      let clock_edge : Edge.t = if p.P.clk_polarity = 1 then Rising else Falling in
      let dffsr set clr d =
        let set = if p.P.set_polarity = 1 then set else ~:set in
        let clr = if p.P.clr_polarity = 1 then clr else ~:clr in
        reg
          (Reg_spec.override
             reg_spec
             ~clock:i.clk
             ~clock_edge
             ~reset:(set |: clr)
             ~reset_to:(mux2 clr gnd vdd))
          ~enable:empty
          d
      in
      O.
        { q =
            concat_lsb
            @@ Array.to_list
            @@ Array.init p.P.width ~f:(fun j ->
              dffsr (bit i.set j) (bit i.clr j) (bit i.d j))
        }
    ;;

    let dffsr = "$dffsr", dffsr
    let cells = [ dffsr ]
    let get_input_width p = I.{ clk = 1; set = p.P.width; clr = p.P.width; d = p.P.width }
    let get_output_width p = O.{ q = p.P.width }
  end

  module Adff = struct
    module P = struct
      type 'a t =
        { width : 'a [@rtlname "WIDTH"]
        ; clk_polarity : 'a [@rtlname "CLK_POLARITY"]
        ; arst_polarity : 'a [@rtlname "ARST_POLARITY"]
        ; arst_value : 'a [@rtlname "ARST_VALUE"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t =
        { clk : 'a [@rtlname "CLK"]
        ; arst : 'a [@rtlname "ARST"]
        ; d : 'a [@rtlname "D"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { q : 'a [@rtlname "Q"] } [@@deriving sexp_of, hardcaml]
    end

    module W = Wrapper (P) (I) (O)

    let adff _ p i =
      let open I in
      let arst_value = pconst (pint p.P.width) p.P.arst_value in
      let p =
        P.map
          ~f:(fun p ->
            try pint p with
            | _ -> 0)
          p
      in
      assert (width i.d = p.P.width);
      let clock_edge : Edge.t = if p.P.clk_polarity = 1 then Rising else Falling in
      let reset_edge : Edge.t = if p.P.arst_polarity = 1 then Rising else Falling in
      let rv = arst_value in
      O.
        { q =
            reg
              (Reg_spec.override
                 reg_spec
                 ~clock:i.clk
                 ~clock_edge
                 ~reset:i.arst
                 ~reset_edge
                 ~reset_to:rv)
              ~enable:empty
              i.d
        }
    ;;

    let adff = "$adff", adff
    let cells = [ adff ]
    let get_input_width p = I.{ clk = 1; arst = 1; d = p.P.width }
    let get_output_width p = O.{ q = p.P.width }
  end

  (* module dlatchsr = struct ... end *)
  (* module fsm = struct ... end *)

  (* module memrd = struct ... end *)
  (* module memwr = struct ... end *)
  (* module meminit = struct ... end *)
  (* module mem = struct ... end *)

  (* must use 'memory -dff' *)
  module Memwr = struct
    module P = struct
      type 'a t =
        { priority : 'a [@rtlname "PRIORITY"]
        ; clk_polarity : 'a [@rtlname "CLK_POLARITY"]
        ; clk_enable : 'a [@rtlname "CLK_ENABLE"]
        ; width : 'a [@rtlname "WIDTH"]
        ; abits : 'a [@rtlname "ABITS"]
        ; memid : 'a [@rtlname "MEMID"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t =
        { en : 'a [@rtlname "EN"]
        ; clk : 'a [@rtlname "CLK"]
        ; data : 'a [@rtlname "DATA"]
        ; addr : 'a [@rtlname "ADDR"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = Hardcaml.Interface.Empty
    module W = Wrapper (P) (I) (O)

    let get_input_width p =
      I.{ en = p.P.width; clk = 1; data = p.P.width; addr = p.P.abits }
    ;;

    let get_output_width _ = O.(map ~f:snd t)

    let memwr _ p i =
      let open I in
      let memid = pstr p.P.memid in
      let p =
        P.map
          ~f:(fun p ->
            try pint p with
            | _ -> 0)
          p
      in
      assert (width i.en = p.P.width);
      assert (width i.clk = 1);
      assert (width i.data = p.P.width);
      assert (width i.addr = p.P.abits);
      let p' =
        P.(
          to_list
          @@ { (map2 ~f:(fun (name, _) x -> Parameter.create ~name ~value:(Int x)) t p) with
               memid = Parameter.create ~name:"MEMID" ~value:(String memid)
             })
      in
      let inst =
        Instantiation.create
          ()
          ~name:"memwr"
          ~parameters:p'
          ~inputs:I.(to_list @@ map2 ~f:(fun (n, _) x -> n, x) t i)
          ~outputs:O.(to_list @@ map2 ~f:(fun (n, _) x -> n, x) t (get_output_width p))
      in
      O.(map ~f:(fun (n, _) -> Map.find_exn inst n) t)
    ;;

    let memwr = "$memwr", memwr
    let cells = [ memwr ]
  end

  module Memrd = struct
    module P = struct
      type 'a t =
        { transparent : 'a [@rtlname "TRANSPARENT"]
        ; clk_polarity : 'a [@rtlname "CLK_POLARITY"]
        ; clk_enable : 'a [@rtlname "CLK_ENABLE"]
        ; width : 'a [@rtlname "WIDTH"]
        ; abits : 'a [@rtlname "ABITS"]
        ; memid : 'a [@rtlname "MEMID"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t =
        { en : 'a [@rtlname "EN"]
        ; clk : 'a [@rtlname "CLK"]
        ; addr : 'a [@rtlname "ADDR"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { data : 'a [@rtlname "DATA"] } [@@deriving sexp_of, hardcaml]
    end

    module W = Wrapper (P) (I) (O)

    let get_input_width p = I.{ en = p.P.width; clk = 1; addr = p.P.abits }
    let get_output_width p = O.{ data = p.P.width }

    let memrd _ p i =
      let open I in
      let memid = pstr p.P.memid in
      let p =
        P.map
          ~f:(fun p ->
            try pint p with
            | _ -> 0)
          p
      in
      assert (width i.en = 1);
      assert (width i.clk = 1);
      assert (width i.addr = p.P.abits);
      let p' =
        P.(
          to_list
          @@ { (map2 ~f:(fun (name, _) x -> Parameter.create ~name ~value:(Int x)) t p) with
               memid = Parameter.create ~name:"MEMID" ~value:(String memid)
             })
      in
      let inst =
        Instantiation.create
          ()
          ~name:"memrd"
          ~parameters:p'
          ~inputs:I.(to_list @@ map2 ~f:(fun (n, _) x -> n, x) t i)
          ~outputs:O.(to_list @@ map2 ~f:(fun (n, _) x -> n, x) t (get_output_width p))
      in
      O.(map ~f:(fun (n, _) -> Map.find_exn inst n) t)
    ;;

    let memrd = "$memrd", memrd
    let cells = [ memrd ]
  end

  (* 'memory -nomap; opt; clean' *)
  module Mem = struct
    module P = struct
      type 'a t =
        { abits : 'a [@rtlname "ABITS"]
        ; init : 'a [@rtlname "INIT"]
        ; memid : 'a [@rtlname "MEMID"]
        ; offset : 'a [@rtlname "OFFSET"]
        ; size : 'a [@rtlname "SIZE"]
        ; width : 'a [@rtlname "WIDTH"]
        ; rd_clk_enable : 'a [@rtlname "RD_CLK_ENABLE"]
        ; rd_clk_polarity : 'a [@rtlname "RD_CLK_POLARITY"]
        ; rd_ports : 'a [@rtlname "RD_PORTS"]
        ; rd_transparent : 'a [@rtlname "RD_TRANSPARENT"]
        ; wr_clk_enable : 'a [@rtlname "WR_CLK_ENABLE"]
        ; wr_clk_polarity : 'a [@rtlname "WR_CLK_POLARITY"]
        ; wr_ports : 'a [@rtlname "WR_PORTS"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t =
        { rd_addr : 'a [@rtlname "RD_ADDR"]
        ; rd_clk : 'a [@rtlname "RD_CLK"]
        ; rd_en : 'a [@rtlname "RD_EN"]
        ; wr_addr : 'a [@rtlname "WR_ADDR"]
        ; wr_clk : 'a [@rtlname "WR_CLK"]
        ; wr_data : 'a [@rtlname "WR_DATA"]
        ; wr_en : 'a [@rtlname "WR_EN"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { rd_data : 'a [@rtlname "RD_DATA"] } [@@deriving sexp_of, hardcaml]
    end

    module W = Wrapper (P) (I) (O)

    let get_input_base_width p =
      I.
        { rd_addr = p.P.abits
        ; rd_clk = 1
        ; rd_en = 1
        ; wr_addr = p.P.abits
        ; wr_clk = 1
        ; wr_data = p.P.width
        ; wr_en = p.P.width
        }
    ;;

    let get_input_ports p =
      I.
        { rd_addr = p.P.rd_ports
        ; rd_clk = p.P.rd_ports
        ; rd_en = p.P.rd_ports
        ; wr_addr = p.P.wr_ports
        ; wr_clk = p.P.wr_ports
        ; wr_data = p.P.wr_ports
        ; wr_en = p.P.wr_ports
        }
    ;;

    let get_input_width p = I.(map2 ~f:( * ) (get_input_base_width p) (get_input_ports p))
    let get_output_base_width p = O.{ rd_data = p.P.width }
    let get_output_ports p = O.{ rd_data = p.P.rd_ports }

    let get_output_width p =
      O.(map2 ~f:( * ) (get_output_base_width p) (get_output_ports p))
    ;;

    let i_to_arrays p i =
      let bwidth = get_input_base_width p in
      let ports = get_input_ports p in
      let to_array x (ports, bwidth) =
        assert (width x = ports * bwidth);
        Array.init ports ~f:(fun j ->
          let l = j * bwidth in
          select x (l + bwidth - 1) l)
      in
      I.(map2 ~f:to_array i (zip ports bwidth))
    ;;

    let get_wren_bits p cell =
      let open Cell_reference in
      let wren = List.Assoc.find_exn cell.inputs "WR_EN" ~equal:String.equal in
      let wren = Array.of_list @@ wren in
      Array.init p.P.wr_ports ~f:(fun j ->
        Array.sub wren ~pos:(p.P.width * j) ~len:p.P.width)
    ;;

    let mem cell p i =
      let p =
        P.map
          ~f:(fun p ->
            try pint p with
            | _ -> 0)
          p
      in
      let i = i_to_arrays p i in
      let module L =
        Lvt.Make_wren (struct
          let dbits = p.P.width
          let abits = p.P.abits
          let size = p.P.size
        end)
      in
      let layout = get_wren_bits p cell in
      let bit x i = (x lsr i) land 1 <> 0 in
      let get_read_mode r =
        match bit p.P.rd_clk_enable r, bit p.P.rd_transparent r with
        | true, true -> `sync_wbr
        | true, false -> `sync_rbw
        | false, true -> `async_wbr
        | false, false -> `async_rbw
      in
      let offset addr = if p.P.offset = 0 then addr else addr -:. p.P.offset in
      let wr_clk w =
        if not (bit p.P.wr_clk_enable w)
        then
          failwith
            ("memory write port is not synchronous: "
             ^ cell.label
             ^ " port "
             ^ Int.to_string w)
        else i.I.wr_clk.(w)
      in
      let spec clk clk_polarity =
        { Signal.reg_clock = clk
        ; reg_clock_edge = (if clk_polarity then Rising else Falling)
        ; reg_reset = empty
        ; reg_reset_edge = Rising
        ; reg_reset_value = empty
        ; reg_clear = empty
        ; reg_clear_level = High
        ; reg_clear_value = empty
        ; reg_enable = empty
        }
      in
      let rd_port r =
        { Lvt.reg_spec = spec i.I.rd_clk.(r) (bit p.P.rd_clk_polarity r)
        ; rd = { ra = offset i.I.rd_addr.(r); re = i.I.rd_en.(r) }
        ; mode = get_read_mode r
        }
      in
      let wr_port w =
        let wspec = spec (wr_clk w) (bit p.P.wr_clk_polarity w) in
        { Lvt.ram_spec = wspec
        ; reg_spec = wspec
        ; wr = { we = i.I.wr_en.(w); wa = offset i.I.wr_addr.(w); d = i.I.wr_data.(w) }
        }
      in
      let q =
        L.memory
          ~layout
          ~rd:(Array.init p.P.rd_ports ~f:rd_port)
          ~wr:(Array.init p.P.wr_ports ~f:wr_port)
      in
      O.{ rd_data = concat_lsb @@ Array.to_list q }
    ;;

    let mem = "$mem", mem
    let cells = [ mem ]
  end

  let cells' =
    List.map ~f:Op1.W.wrapper Op1.cells
    @ List.map ~f:Op2.W.wrapper Op2.cells
    @ List.map ~f:Fa.W.wrapper Fa.cells
    @ List.map ~f:Lcu.W.wrapper Lcu.cells
    @ List.map ~f:Fa.W.wrapper Fa.cells
    @ List.map ~f:Slice.W.wrapper Slice.cells
    @ List.map ~f:Mux.W.wrapper Mux.cells
    @ List.map ~f:Pmux.W.wrapper Pmux.cells
    @ List.map ~f:Lut.W.wrapper Lut.cells
    @ List.map ~f:Dff.W.wrapper Dff.cells
    @ List.map ~f:Dffe.W.wrapper Dffe.cells
    @ List.map ~f:Dffsr.W.wrapper Dffsr.cells
    @ List.map ~f:Adff.W.wrapper Adff.cells
    @ (*(List.map Memwr.W.wrapper Memwr.cells) @
        (List.map Memrd.W.wrapper Memrd.cells) @*)
    List.map ~f:Mem.W.wrapper Mem.cells
  ;;

  let blackboxes =
    [ "shfitx"
    ; "macc"
    ; "div"
    ; "mod"
    ; "pow"
    ; "alu"
    ; "tribuf"
    ; "assert"
    ; "assume"
    ; "equiv"
    ; "sr"
    ; "dlatch"
    ; "dlatchsr"
    ; "memrd"
    ; "memwr"
    ; "meminit"
    ]
  ;;

  let cells = { blackboxes; cell_fns = cells' }
end

module Proof = struct
  module type Cells = sig
    module P : Hardcaml.Interface.S
    module I : Hardcaml.Interface.S
    module O : Hardcaml.Interface.S
    module W : module type of Simlib.Wrapper (P) (I) (O)

    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Make (C : Cells) = struct
    let cell_name name = String.sub name ~pos:1 ~len:(String.length name - 1)
    let tech_name name = "techlib_" ^ cell_name name

    let proof ?(path = "") ?(postfix = "") params (name, fn) =
      let open Hardcaml.Signal in
      (*let _ = C.P.(map2 (fun (n,_) x -> printf "%s: %i\n%!" n x) t params) in*)

      (* circuit inputs *)
      let inputs =
        let widths = C.get_input_width params in
        C.I.(map2 ~f:(fun (n, _) w -> input n w) t widths)
      in
      let no_cell =
        Cell_reference.
          { typ = ""; label = ""; parameters = []; inputs = []; outputs = [] }
      in
      (* hardcaml simlib circuit *)
      let mkparams p =
        C.P.(
          map2 ~f:(fun (name, _) value -> Parameter.create ~name ~value:(Int value)) t p)
      in
      let outputs = fn no_cell (mkparams params) inputs in
      (*let () = printf "created %s\n" name in*)

      (* instantiate yosys techlib module *)
      let outputs_chk =
        let outputs = C.get_output_width params in
        let inst =
          Hardcaml.Instantiation.create
            ()
            ~name:(tech_name name)
            ~parameters:C.P.(to_list @@ mkparams params)
            ~inputs:C.I.(to_list @@ map2 ~f:(fun (n, _) i -> n, i) t inputs)
            ~outputs:C.O.(to_list @@ map2 ~f:(fun (n, _) o -> n, o) t outputs)
        in
        C.O.(map ~f:(fun (n, _) -> Map.find_exn inst n) t)
      in
      (*let () = printf "created %s\n" (tech_name name) in*)

      (* compare outputs *)
      let check =
        concat_msb (C.O.to_list outputs) <>: concat_msb (C.O.to_list outputs_chk)
      in
      (* write the test circuit *)
      let name = "sat_" ^ cell_name name ^ postfix in
      let fname = Filename.concat path (name ^ ".v") in
      let yosys =
        sprintf
          "%s -p 'read_verilog -sv test/simlib_chk.v %s; hierarchy -top %s; proc; \
           flatten; sat -prove check 0 -set-def check -set-def-inputs -verify %s' -q -q \
           2>/dev/null || echo %s failed"
          Run.yosys_exe
          fname
          name
          name
          name
      in
      let circ = Hardcaml.Circuit.create_exn ~name [ output "check" check ] in
      let f = Out_channel.create fname in
      fprintf f "// %s\n" yosys;
      Hardcaml.Rtl.output Verilog circ ~output_mode:(To_channel f);
      Out_channel.close f;
      printf "%s\n%!" yosys
    ;;
  end
end
