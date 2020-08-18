module simple_adder8 (
  input [7:0] a, b,
  output [7:0] c
);

  assign c = a + b;

endmodule

module simple_adder16 (
  input [15:0] a, b,
  output [15:0] c
);

  assign c = a + b;

endmodule

module simple_adder #(parameter N=8) (
  input [N-1:0] a, b,
  output [N-1:0] c
);

  assign c = a + b;

endmodule

module carry_save_adder (
  input [3:0] a, b,
  output [4:0] c
);
  wire [2:0] co;
  fa the_fa0 ( .a(a[0]), .b(b[0]), .s(c[0]), .ci(1'b0),  .co(co[0]) );
  fa the_fa1 ( .a(a[1]), .b(b[1]), .s(c[1]), .ci(co[0]), .co(co[1]) );
  fa the_fa2 ( .a(a[2]), .b(b[2]), .s(c[2]), .ci(co[1]), .co(co[2]) );
  fa the_fa3 ( .a(a[3]), .b(b[3]), .s(c[3]), .ci(co[2]), .co(c[4])  );
endmodule
