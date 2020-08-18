// A simple test design which we can simulate through the yosys->hardcaml flow

module counter (
  input clk, rst, clr, en,
  output reg [7:0] q
);
  reg [7:0] c;

  always @(posedge clk, posedge rst) begin
    if (rst) c <= 0;
    else if (clr) c <= 0;
    else if (en) c <= c + 1;
  end

  assign q = c;

endmodule
  
