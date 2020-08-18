open! Import
open! Interface

module I = struct
  type 'a t = { a : 'a } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { b : 'a } [@@deriving sexp_of, hardcaml]
end

module Conv = Make (I) (O)

let verilog = {| module testme (input [2:0] a, output b); assign b = !a; endmodule |}

let%expect_test "load simple design" =
  let tmp = Filename.temp_file "hardcaml_of_verilog_interface_" "test" in
  Out_channel.write_all tmp ~data:verilog;
  let f =
    unstage (Conv.convert_exn () ~blackboxes:[] ~verilog:[ tmp ] ~topname:"testme")
  in
  Unix.unlink tmp;
  require_does_not_raise [%here] (fun () ->
    print_s [%sexp (f { a = Hardcaml.Signal.wire 3 } : _ O.t)]);
  [%expect {|
    ((b _)) |}]
;;
