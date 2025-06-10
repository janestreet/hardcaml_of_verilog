open! Core
open Hardcaml
open Signal
open Hardcaml_of_verilog

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; a : 'a [@bits 1]
    ; b : 'a [@bits 1]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { y : 'a [@bits 1] } [@@deriving hardcaml]
end

let create (i : _ I.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  { O.y = reg spec ~enable:vdd (i.a +: i.b) }
;;

let%expect_test "simple adder to json" =
  let module Circuit = Hardcaml.Circuit.With_interface (I) (O) in
  let circuit = Circuit.create_exn ~name:"adder" create in
  let json =
    Circuit_to_json.convert ~debug:true circuit |> Expert.Yosys_netlist.to_string_hum
  in
  Out_channel.print_string json;
  [%expect
    {|
    (!ignore_set (11))
    (!driver_map ((2 1) (4 3) (6 5) (8 7) (9 12)))
    (!select_map ())
    Wire[id:9 bits:1 names:y deps:12] -> 12
    Reg[id:12 bits:1 names: deps:10,4,2,11]
    Op[id:10 bits:1 names: deps:8,6] = add
    Wire[id:8 bits:1 names: deps:7] -> 7
    Wire[id:7 bits:1 names:a deps:] -> ()
    Wire[id:6 bits:1 names: deps:5] -> 5
    Wire[id:5 bits:1 names:b deps:] -> ()
    Wire[id:4 bits:1 names: deps:3] -> 3
    Wire[id:3 bits:1 names:clock deps:] -> ()
    Wire[id:2 bits:1 names: deps:1] -> 1
    Wire[id:1 bits:1 names:clear deps:] -> ()
    Const[id:11 bits:1 names: deps:] = 0
    {
      "creator": "hardcaml",
      "modules": {
        "adder": {
          "ports": {
            "clear": {
              "direction": "input",
              "bits": [
                1
              ]
            },
            "clock": {
              "direction": "input",
              "bits": [
                3
              ]
            },
            "b": {
              "direction": "input",
              "bits": [
                5
              ]
            },
            "a": {
              "direction": "input",
              "bits": [
                7
              ]
            },
            "y": {
              "direction": "output",
              "bits": [
                12
              ]
            }
          },
          "cells": {
            "$gate10": {
              "hide_name": 0,
              "type": "$add",
              "parameters": {},
              "port_directions": {
                "A": "input",
                "B": "input",
                "Y": "output"
              },
              "connections": {
                "A": [
                  7
                ],
                "B": [
                  5
                ],
                "Y": [
                  10
                ]
              }
            },
            "$procdff$12": {
              "hide_name": 0,
              "type": "$our_dff",
              "parameters": {},
              "port_directions": {
                "CLK": "input",
                "CE": "input",
                "CLR": "input",
                "RST": "input",
                "D": "input",
                "Q": "output"
              },
              "connections": {
                "D": [
                  10
                ],
                "CLR": [
                  1
                ],
                "RST": [
                  0
                ],
                "CLK": [
                  3
                ],
                "CE": [
                  0
                ],
                "Q": [
                  12
                ]
              }
            }
          },
          "netnames": {}
        }
      }
    }
    |}]
;;
