open! Core
open Hardcaml_of_verilog

let of_verilog ?verbose ?(netlist = false) ?passes verilog =
  let verilog_file = Stdlib.Filename.temp_file "test" ".v" in
  Stdio.Out_channel.write_all verilog_file ~data:verilog;
  let verilog_design =
    Verilog_design.create
      ~top:(Verilog_design.Module.create ~module_name:"top" ~path:verilog_file ())
      ()
  in
  let to_netlist () = Netlist.create ?verbose ?passes verilog_design in
  let to_circuit () =
    let%bind.Or_error netlist = to_netlist () in
    let%bind.Or_error t =
      Verilog_circuit.create netlist ~top_name:(Verilog_design.top_name verilog_design)
    in
    let%bind.Or_error circuit = Verilog_circuit.to_hardcaml_circuit t in
    Hardcaml.Rtl.print Verilog circuit;
    Ok ()
  in
  if netlist then print_s [%message (to_netlist () : Netlist.t Or_error.t)];
  print_s [%message (to_circuit () : unit Or_error.t)]
;;

let%expect_test "no module output driver" =
  of_verilog
    ~netlist:true
    {|
module top (input a, output b); endmodule
|};
  [%expect
    {|
    ("to_netlist ()" (Ok (top)))
    ("to_circuit ()" (Error ("Failed to find net in bus map" (i 3))))
    |}]
;;

let%expect_test "simple assignment" =
  of_verilog
    {|
module top (input a, output b); assign b = a; endmodule
|};
  [%expect
    {|
    module top (
        a,
        b
    );

        input a;
        output b;

        wire a_0;
        wire b_0;
        assign a_0 = a;
        assign b_0 = a_0;
        assign b = a_0;

    endmodule
    ("to_circuit ()" (Ok ()))
    |}]
;;

let%expect_test "simple xor of inputs" =
  of_verilog
    {|
module top (input a, output b);
  wire x;
  assign x = a;
  assign b = x ^ x;
endmodule
|};
  [%expect
    {|
    module top (
        a,
        b
    );

        input a;
        output b;

        wire a_0;
        wire x;
        wire _5;
        wire b_0;
        assign a_0 = a;
        assign _5 = a_0 ^ a_0;
        assign b_0 = _5;
        assign x = a_0;
        assign b = b_0;

    endmodule
    ("to_circuit ()" (Ok ()))
    |}]
;;

let%expect_test "unused input bits" =
  of_verilog
    {|
module top (input [1:0] a, output b);
  wire x;
  assign x = a[0];
  assign b = x + 1;
endmodule
|};
  [%expect
    {|
    module top (
        a,
        b
    );

        input [1:0] a;
        output b;

        wire [31:0] _8;
        wire [1:0] a_0;
        wire x;
        wire [30:0] _5;
        wire [31:0] _7;
        wire [31:0] _9;
        wire [31:0] _3;
        wire b_0;
        assign _8 = 32'b00000000000000000000000000000001;
        assign a_0 = a;
        assign x = a_0[0:0];
        assign _5 = 31'b0000000000000000000000000000000;
        assign _7 = { _5,
                      x };
        assign _9 = _7 + _8;
        assign _3 = _9;
        assign b_0 = _3[0:0];
        assign b = b_0;

    endmodule
    ("to_circuit ()" (Ok ()))
    |}]
;;

let%expect_test "instantiate module" =
  let verilog =
    {|
module foo (input a, b, output c); assign c = a & b; endmodule
module top (input a, b, output c);
    foo the_foo (.a(a), .b(b), .c(c));
endmodule
|}
  in
  (* flattened by yosys *)
  of_verilog verilog;
  [%expect
    {|
    module top (
        b,
        a,
        c
    );

        input b;
        input a;
        output c;

        wire b_0;
        wire the_foo_b;
        wire a_0;
        wire the_foo_a;
        wire _7;
        wire c_0;
        wire the_foo_c;
        assign b_0 = b;
        assign a_0 = a;
        assign _7 = a_0 & b_0;
        assign c_0 = _7;
        assign the_foo_b = b_0;
        assign the_foo_a = a_0;
        assign the_foo_c = c_0;
        assign c = c_0;

    endmodule
    ("to_circuit ()" (Ok ()))
    |}];
  (* keep hierarchy *)
  of_verilog ~passes:[ Proc; Opt { mux_undef = false }; Clean ] verilog;
  [%expect
    {|
    module top (
        a,
        b,
        c
    );

        input a;
        input b;
        output c;

        wire a_0;
        wire b_0;
        wire _8;
        wire _5;
        wire c_0;
        assign a_0 = a;
        assign b_0 = b;
        foo
            the_foo
            ( .b(b_0),
              .a(a_0),
              .c(_8) );
        assign _5 = _8;
        assign c_0 = _5;
        assign c = c_0;

    endmodule
    ("to_circuit ()" (Ok ()))
    |}]
;;
