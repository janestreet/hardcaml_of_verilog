(** Hardcaml implementation of the Yosys Simlib tech library. *)

open! Import
open Hardcaml

exception Invalid_parameter of string
exception Invalid_input of string

(** Function which provides the implementation for a cell in the tech library *)
module Cell_implementation : sig
  type t = Parameter.t list -> (string * Signal.t) list -> (string * Signal.t) list
end

(** Data structure containing implementations for each cell in the tech library. *)
type t =
  { blackboxes : string list
  (** Known cell references for which we have no implementation, but should always
      generate hardcaml instantiations for. *)
  ; cell_fns : (string * (Cell_reference.t -> Cell_implementation.t)) list
  (** Implementations for each cell in the tech library *)
  }

module Simlib : sig
  module Wrapper
      (P : Hardcaml.Interface.S)
      (I : Hardcaml.Interface.S)
      (O : Hardcaml.Interface.S) : sig
    type fn =
      string * (Cell_reference.t -> Parameter.t P.t -> Signal.t I.t -> Signal.t O.t)

    val wrapper : fn -> string * (Cell_reference.t -> Cell_implementation.t)
  end

  module Op1 : sig
    module P : sig
      type 'a t =
        { a_signed : 'a
        ; a_width : 'a
        ; y_width : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t = { a : 'a } [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { y : 'a } [@@deriving sexp_of, hardcaml]
    end

    module W : module type of Wrapper (P) (I) (O)

    val not_ : W.fn
    val pos : W.fn
    val neg : W.fn
    val reduce_or : W.fn
    val reduce_and : W.fn
    val reduce_xor : W.fn
    val reduce_xnor : W.fn
    val reduce_bool : W.fn
    val logic_not : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Op2 : sig
    module P : sig
      type 'a t =
        { a_signed : 'a
        ; b_signed : 'a
        ; a_width : 'a
        ; b_width : 'a
        ; y_width : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t =
        { a : 'a
        ; b : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { y : 'a } [@@deriving sexp_of, hardcaml]
    end

    module W : module type of Wrapper (P) (I) (O)

    val and_ : W.fn
    val or_ : W.fn
    val xor_ : W.fn
    val xnor_ : W.fn
    val add : W.fn
    val sub : W.fn
    val mul : W.fn
    val shl : W.fn
    val shr : W.fn
    val sshl : W.fn
    val sshr : W.fn
    val shift : W.fn

    (* val shiftx, macc, div, mod, pow *)

    val logic_and : W.fn
    val logic_or : W.fn
    val lt : W.fn
    val le : W.fn
    val gt : W.fn
    val ge : W.fn
    val eq : W.fn
    val ne : W.fn
    val eqx : W.fn
    val nex : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Fa : sig
    module P : sig
      type 'a t = { width : 'a } [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t =
        { a : 'a
        ; b : 'a
        ; c : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { x : 'a
        ; y : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module W : module type of Wrapper (P) (I) (O)

    val fa : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Lcu : sig
    module P : sig
      type 'a t = { width : 'a } [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t =
        { p : 'a
        ; g : 'a
        ; ci : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { co : 'a } [@@deriving sexp_of, hardcaml]
    end

    module W : module type of Wrapper (P) (I) (O)

    val lcu : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  (* module Alu : sig ... end *)

  module Slice : sig
    module P : sig
      type 'a t =
        { offset : 'a
        ; a_width : 'a
        ; y_width : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t = { a : 'a } [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { y : 'a } [@@deriving sexp_of, hardcaml]
    end

    module W : module type of Wrapper (P) (I) (O)

    val slice : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Concat : sig
    module P : sig
      type 'a t =
        { a_width : 'a
        ; b_width : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t =
        { a : 'a
        ; b : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { y : 'a } [@@deriving sexp_of, hardcaml]
    end

    module W : module type of Wrapper (P) (I) (O)

    val concat : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Mux : sig
    module P : sig
      type 'a t = { width : 'a } [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t =
        { a : 'a
        ; b : 'a
        ; s : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { y : 'a } [@@deriving sexp_of, hardcaml]
    end

    module W : module type of Wrapper (P) (I) (O)

    val mux : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Pmux : sig
    module P : sig
      type 'a t =
        { width : 'a
        ; s_width : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t =
        { a : 'a
        ; b : 'a
        ; s : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { y : 'a } [@@deriving sexp_of, hardcaml]
    end

    module W : module type of Wrapper (P) (I) (O)

    val pmux : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Lut : sig
    module P : sig
      type 'a t =
        { width : 'a
        ; lut : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t = { a : 'a } [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { y : 'a } [@@deriving sexp_of, hardcaml]
    end

    module W : module type of Wrapper (P) (I) (O)

    val lut : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Dff : sig
    module P : sig
      type 'a t =
        { width : 'a
        ; clk_polarity : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t =
        { clk : 'a
        ; d : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { q : 'a } [@@deriving sexp_of, hardcaml]
    end

    module W : module type of Wrapper (P) (I) (O)

    val dff : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Dffe : sig
    module P : sig
      type 'a t =
        { width : 'a
        ; clk_polarity : 'a
        ; en_polarity : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t =
        { clk : 'a
        ; en : 'a
        ; d : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { q : 'a } [@@deriving sexp_of, hardcaml]
    end

    module W : module type of Wrapper (P) (I) (O)

    val dffe : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Dffsr : sig
    module P : sig
      type 'a t =
        { width : 'a
        ; clk_polarity : 'a
        ; set_polarity : 'a
        ; clr_polarity : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t =
        { clk : 'a
        ; set : 'a
        ; clr : 'a
        ; d : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { q : 'a } [@@deriving sexp_of, hardcaml]
    end

    module W : module type of Wrapper (P) (I) (O)

    val dffsr : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Adff : sig
    module P : sig
      type 'a t =
        { width : 'a
        ; clk_polarity : 'a
        ; arst_polarity : 'a
        ; arst_value : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t =
        { clk : 'a
        ; arst : 'a
        ; d : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { q : 'a } [@@deriving sexp_of, hardcaml]
    end

    module W : module type of Wrapper (P) (I) (O)

    val adff : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Memwr : sig
    module P : sig
      type 'a t =
        { priority : 'a
        ; clk_polarity : 'a
        ; clk_enable : 'a
        ; width : 'a
        ; abits : 'a
        ; memid : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t =
        { en : 'a
        ; clk : 'a
        ; data : 'a
        ; addr : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : Hardcaml.Interface.Empty
    module W : module type of Wrapper (P) (I) (O)

    val memwr : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Memrd : sig
    module P : sig
      type 'a t =
        { transparent : 'a
        ; clk_polarity : 'a
        ; clk_enable : 'a
        ; width : 'a
        ; abits : 'a
        ; memid : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t =
        { en : 'a
        ; clk : 'a
        ; addr : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { data : 'a } [@@deriving sexp_of, hardcaml]
    end

    module W : module type of Wrapper (P) (I) (O)

    val memrd : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Mem : sig
    module P : sig
      type 'a t =
        { abits : 'a
        ; init : 'a
        ; memid : 'a
        ; offset : 'a
        ; size : 'a
        ; width : 'a
        ; rd_clk_enable : 'a
        ; rd_clk_polarity : 'a
        ; rd_ports : 'a
        ; rd_transparent : 'a
        ; wr_clk_enable : 'a
        ; wr_clk_polarity : 'a
        ; wr_ports : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I : sig
      type 'a t =
        { rd_addr : 'a
        ; rd_clk : 'a
        ; rd_en : 'a
        ; wr_addr : 'a
        ; wr_clk : 'a
        ; wr_data : 'a
        ; wr_en : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { rd_data : 'a } [@@deriving sexp_of, hardcaml]
    end

    module W : module type of Wrapper (P) (I) (O)

    val mem : W.fn
    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  (** Yosys tech library *)
  val cells : t
end

module Proof : sig
  module type Cells = sig
    module P : Hardcaml.Interface.S
    module I : Hardcaml.Interface.S
    module O : Hardcaml.Interface.S
    module W : module type of Simlib.Wrapper (P) (I) (O)

    val cells : W.fn list
    val get_input_width : int P.t -> int I.t
    val get_output_width : int P.t -> int O.t
  end

  module Make (C : Cells) : sig
    val proof : ?path:string -> ?postfix:string -> int C.P.t -> C.W.fn -> unit
  end
end
