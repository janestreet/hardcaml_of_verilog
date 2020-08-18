// async 
module async_1rd_1wr (
  input clk,
  input [7:0] a, 
  input we, 
  input [15:0] d,
  output [15:0] q
);
  reg [15:0] mem[0:255];
  always @(posedge clk) if (we) mem[a] <= d;
  assign q = mem[a];
endmodule

// sync 
module sync_1rd_1wr (
  input clk,
  input [7:0] a, 
  input we, 
  input [15:0] d,
  output reg [15:0] q
);
  reg [15:0] mem[0:255];
  always @(posedge clk) if (we) mem[a] <= d;
  always @(posedge clk) q <= mem[a];
endmodule

// sync (transparent) 
module sync_1rd_1wr_t (
  input clk,
  input [7:0] a, 
  input we, 
  input [15:0] d,
  output [15:0] q
);
  reg [15:0] mem[0:255];
  reg [7:0] ra;
  always @(posedge clk) if (we) mem[a] <= d;
  always @(posedge clk) ra <= a;
  assign q = mem[ra];
endmodule

// sync with byte enables
module sync_1rd_1wr_byteen (
  input clk,
  input [7:0] a, 
  input [1:0] we, 
  input [15:0] d,
  output reg [15:0] q
);
  reg [15:0] mem[0:255];
  always @(posedge clk) begin
    if (we[0]) mem[a][7:0] <= d[7:0];
    if (we[1]) mem[a][15:8] <= d[15:8];
  end
  always @(posedge clk) q <= mem[a];
endmodule

// sync with bit enables
module sync_1rd_1wr_biten (
  input clk,
  input [7:0] a, 
  input [15:0] we, 
  input [15:0] d,
  output reg [15:0] q
);
  reg [15:0] mem[0:255];
  integer i;
  always @(posedge clk) begin
    for (i=0; i<16; i=i+1) begin
      if (we[i]) mem[a][i] <= d[i];
    end
  end
  always @(posedge clk) q <= mem[a];
endmodule

// sync 2rd 2wr
module sync_2rd_2wr (
  input clk,
  input [7:0] ra0, 
  input [7:0] ra1, 
  input [7:0] wa0, 
  input [7:0] wa1, 
  input [1:0] we, 
  input [1:0] re, 
  input [15:0] d0,
  input [15:0] d1,
  output reg [15:0] q0,
  output reg [15:0] q1
);
  reg [15:0] mem[0:255];
  integer i;
  always @(posedge clk) begin
    if (we0) mem[wa0] <= d0;
    if (we1) mem[wa1] <= d1;
  end
  always @(posedge clk) if (re[0]) q0 <= mem[ra0];
  always @(posedge clk) if (re[1]) q1 <= mem[ra1];
endmodule

