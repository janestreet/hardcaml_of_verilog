module adff_test (
  input clk, reset_n,
  input [100:0] d,
  output reg [100:0] q
);

  always @(posedge clk, negedge reset_n)
    if (!reset_n) q <= 1;
    else q <= d;

endmodule
