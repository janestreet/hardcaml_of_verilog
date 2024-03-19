open Core

let top = Test_verilog_design.top

let%expect_test "yosys script" =
  let script =
    Hardcaml_of_verilog.Expert.Synthesize.yosys_script top ~json_file:"out.json"
  in
  Out_channel.print_string script;
  [%expect
    {|
    read_verilog  -defer -lib vlog/bar.v
    read_verilog  -defer vlog/brumble.v
    read_verilog  -defer vlog/fudge.v
    read_verilog  -defer vlog/foo.v
    chparam -set A 1 -set B "popcorn" foo
    hierarchy -top foo
    proc
    flatten
    memory -nomap
    opt
    clean
    opt -mux_undef
    clean
    write_json out.json
    |}]
;;

let%expect_test "custom passes" =
  let script =
    Hardcaml_of_verilog.Expert.Synthesize.yosys_script
      ~passes:[ Clean ]
      top
      ~json_file:"out.json"
  in
  Out_channel.print_string script;
  [%expect
    {|
    read_verilog  -defer -lib vlog/bar.v
    read_verilog  -defer vlog/brumble.v
    read_verilog  -defer vlog/fudge.v
    read_verilog  -defer vlog/foo.v
    chparam -set A 1 -set B "popcorn" foo
    hierarchy -top foo
    clean
    write_json out.json
    |}]
;;
