/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.6.3. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module topEntity
    ( // Inputs
      input  clk // clock
    , input  rst // reset
    , input  en // enable
    , input  c$arg

      // Outputs
    , output wire  result
    );
  wire  c$s1_app_arg;
  reg [5:0] s1 = {5'd3,   1'b1};
  wire [4:0] acc;
  wire  st;

  assign c$s1_app_arg = (c$arg & (acc == 5'd0)) ? (~ st) : st;

  assign result = s1[0:0];

  // register begin
  always @(posedge clk or  posedge  rst) begin : s1_register
    if ( rst) begin
      s1 <= {5'd3,   1'b1};
    end else if (en) begin
      s1 <= {(acc + 5'd1) % 5'd3,   c$s1_app_arg};
    end
  end
  // register end

  assign acc = s1[5:1];

  assign st = s1[0:0];


endmodule

