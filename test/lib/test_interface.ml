open! Core
open Hardcaml_of_verilog
open Expect_test_helpers_core

module I = struct
  type 'a t = { a : 'a } [@@deriving hardcaml]
end

module O = struct
  type 'a t = { b : 'a } [@@deriving hardcaml]
end

module Conv = With_interface.Make (I) (O)

let verilog = {| module testme (input [2:0] a, output b); assign b = !a; endmodule |}

let%expect_test "load simple design" =
  let tmp = Filename_unix.temp_file "hardcaml_of_verilog_interface_" ".v" in
  Out_channel.write_all tmp ~data:verilog;
  let f =
    Conv.create
      Verilog_design.(create ~top:(Module.create ~module_name:"testme" ~path:tmp ()) ())
  in
  Core_unix.unlink tmp;
  require_does_not_raise (fun () ->
    let f = Or_error.ok_exn f in
    let o = f { a = Hardcaml.Signal.wire 3 } |> Or_error.ok_exn in
    print_s [%message (o : _ O.t)]);
  [%expect {| (o ((b _))) |}]
;;
