// See what happens with black boxes
(* blackbox *)
module bbox #(
  parameter x=1
) (
  input a, b,
  output [x:0] c
);

endmodule

module top (
  input a, b,
  output [1:0] c,
  output [2:0] d,
  output reg e
);

  bbox #(.x(1)) the_bbox1 (.a(a), .b(b), .c(c));
  bbox #(.x(2)) the_bbox2 (.a(a), .b(b), .c(d));

  // infer a dlatch

  always @*
    if (a) e = b;

endmodule
