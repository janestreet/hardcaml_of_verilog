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
        wire signal_xor;
        wire b_0;
        assign a_0 = a;
        assign signal_xor = a_0 ^ a_0;
        assign b_0 = signal_xor;
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

        wire [31:0] signal_const;
        wire [1:0] a_0;
        wire x;
        wire [30:0] signal_const_1;
        wire [31:0] signal_cat;
        wire [31:0] signal_add;
        wire [31:0] signal_wire;
        wire b_0;
        assign signal_const = 32'b00000000000000000000000000000001;
        assign a_0 = a;
        assign x = a_0[0:0];
        assign signal_const_1 = 31'b0000000000000000000000000000000;
        assign signal_cat = { signal_const_1,
                              x };
        assign signal_add = signal_cat + signal_const;
        assign signal_wire = signal_add;
        assign b_0 = signal_wire[0:0];
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
        wire \the_foo.b ;
        wire a_0;
        wire \the_foo.a ;
        wire signal_and;
        wire c_0;
        wire \the_foo.c ;
        assign b_0 = b;
        assign a_0 = a;
        assign signal_and = a_0 & b_0;
        assign c_0 = signal_and;
        assign \the_foo.b  = b_0;
        assign \the_foo.a  = a_0;
        assign \the_foo.c  = c_0;
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
        wire signal_inst;
        wire signal_wire;
        wire c_0;
        assign a_0 = a;
        assign b_0 = b;
        foo
            the_foo
            ( .b(b_0),
              .a(a_0),
              .c(signal_inst) );
        assign signal_wire = signal_inst;
        assign c_0 = signal_wire;
        assign c = c_0;

    endmodule
    ("to_circuit ()" (Ok ()))
    |}]
;;
