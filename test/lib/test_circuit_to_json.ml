open! Core
open Hardcaml
open Hardcaml_of_verilog
open Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; a : 'a [@bits 1]
    ; b : 'a [@bits 1]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { y : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
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
    (!ignore_set (11 12))
    (!driver_map ((2 1) (4 3) (6 5) (8 7) (9 14)))
    (!select_map ())
    Wire[id:9 bits:1 names:y deps:14] -> 14
    Reg[id:14 bits:1 names: deps:10,4,0,11,2,12,13]
    Op[id:10 bits:1 names: deps:8,6] = add
    Wire[id:8 bits:1 names: deps:7] -> 7
    Wire[id:7 bits:1 names:a deps:0] -> 0
    Empty
    Wire[id:6 bits:1 names: deps:5] -> 5
    Wire[id:5 bits:1 names:b deps:0] -> 0
    Wire[id:4 bits:1 names: deps:3] -> 3
    Wire[id:3 bits:1 names:clock deps:0] -> 0
    Const[id:11 bits:1 names: deps:] = 0
    Wire[id:2 bits:1 names: deps:1] -> 1
    Wire[id:1 bits:1 names:clear deps:0] -> 0
    Const[id:12 bits:1 names: deps:] = 0
    Const[id:13 bits:1 names:vdd deps:] = 1
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
                14
              ]
            }
          },
          "cells": {
            "$vdd_13": {
              "hide_name": 0,
              "type": "$vdd_13",
              "parameters": {},
              "port_directions": {
                "Y": "output"
              },
              "connections": {
                "Y": [
                  13
                ]
              }
            },
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
            "$procdff$14": {
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
                  13
                ],
                "Q": [
                  14
                ]
              }
            }
          },
          "netnames": {}
        }
      }
    } |}]
;;
