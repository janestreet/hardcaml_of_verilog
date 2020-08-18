open! Import
open! Synthesize

let tempfile ext = Filename.temp_file "hardcaml_of_verilog_synthesize_" ext

let%expect_test "[to_json_netlist]" =
  let verilog = {| module foo(input a, output b); assign b = !a; endmodule |} in
  let tmp = tempfile ".v" in
  let tmp_pattern = String.Search_pattern.create tmp in
  Out_channel.write_all tmp ~data:verilog;
  let json_string =
    Synthesize.convert_to_json_string
      ~params:[]
      ~topname:"foo"
      ~blackboxes:[]
      ~verilog:[ tmp ]
  in
  Unix.unlink tmp;
  (match json_string with
   | Ok json_string ->
     print_endline
       (String.Search_pattern.replace_all tmp_pattern ~in_:json_string ~with_:"<tmp.v>")
   | Error error -> print_s [%message (error : Error.t)]);
  [%expect
    {|
    {
      "creator": "Yosys 0.9 (git sha1 UNKNOWN, gcc 4.8.5 -fPIC -Os)",
      "modules": {
        "foo": {
          "attributes": {
            "top": 1,
            "src": "<tmp.v>:1"
          },
          "ports": {
            "a": {
              "direction": "input",
              "bits": [ 2 ]
            },
            "b": {
              "direction": "output",
              "bits": [ 3 ]
            }
          },
          "cells": {
            "$logic_not$<tmp.v>:1$1": {
              "hide_name": 1,
              "type": "$logic_not",
              "parameters": {
                "A_SIGNED": 0,
                "A_WIDTH": 1,
                "Y_WIDTH": 1
              },
              "attributes": {
                "src": "<tmp.v>:1"
              },
              "port_directions": {
                "A": "input",
                "Y": "output"
              },
              "connections": {
                "A": [ 2 ],
                "Y": [ 3 ]
              }
            }
          },
          "netnames": {
            "a": {
              "hide_name": 0,
              "bits": [ 2 ],
              "attributes": {
                "src": "<tmp.v>:1"
              }
            },
            "b": {
              "hide_name": 0,
              "bits": [ 3 ],
              "attributes": {
                "src": "<tmp.v>:1"
              }
            }
          }
        }
      }
    } |}]
;;
