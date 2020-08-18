module ha (
  input a, b,
  output c, s
);
  assign s = a ^ b;
  assign c = a & b;
endmodule

module fa (
  input a, b, ci,
  output s, co
);
  wire sx;
  ha the_ha0 ( .a(a), .b(b), .c(cx), .s(sx) );
  ha the_ha1 ( .a(sx), .b(ci), .c(cy), .s(s) );
  assign co = cx | cy;
endmodule
