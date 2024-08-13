open Core
open Hardcaml_of_verilog

let header_json =
  {|
  { "creator" : "yosys",
    "modules" : {}
  }
|}
;;

let basic_json =
  {|
{ "creator" : "yosys",
  "modules" :
  { "foo":
    { "ports": { "clock": { "direction": "input", "bits": [ 2, 3, 4 ] },
                 "q": { "direction": "output", "bits": [ "1", "0", 0, 1 ]}
               },
      "cells": { "mymodule": { "hide_name": 1,
                               "type": "foo",
                               "parameters": {},
                               "port_directions": { "A": "input", "B": "output" },
                               "connections": { "A": [ 1, "1", 12 ], "B": [ ] }
                             }
               },
      "netnames":
        { "fudge":
           { "hide_name": 0,
             "bits": [ 200 ],
             "attributes": { "src": "/home/andyman/foo.v:1001" }
           }
        }
    },
    "bar":
    { "ports": { },
      "cells": { },
      "netnames": { }
    }
  }
}
|}
;;

let parse json = Expert.Yosys_netlist.of_string json

let%expect_test "basic header" =
  let t = parse header_json in
  print_s [%message (t : Expert.Yosys_netlist.t Or_error.t)];
  [%expect {| (t (Ok ((creator yosys) (modules ())))) |}]
;;

let%expect_test "basic yosys json" =
  let t = parse basic_json in
  print_s [%message (t : Expert.Yosys_netlist.t Or_error.t)];
  [%expect
    {|
    (t
     (Ok
      ((creator yosys)
       (modules
        (((name foo)
          (value
           ((ports
             (((name clock)
               (value ((direction Input) (bits ((Index 2) (Index 3) (Index 4))))))
              ((name q)
               (value ((direction Output) (bits (Vdd Gnd (Index 0) (Index 1))))))))
            (cells
             (((name mymodule)
               (value
                ((hide_name 1) (module_name foo) (parameters ())
                 (port_directions
                  (((name A) (value Input)) ((name B) (value Output))))
                 (connections
                  (((name A) (value ((Index 1) Vdd (Index 12))))
                   ((name B) (value ())))))))))
            (netnames
             (((name fudge) (value ((hide_name 0) (bits ((Index 200)))))))))))
         ((name bar) (value ((ports ()) (cells ()) (netnames ())))))))))
    |}]
;;
