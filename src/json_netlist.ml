open! Import

type t = Yosys_atd_t.t

let of_string t =
  let st = Yojson.init_lexer () in
  let buf = Lexing.from_string t in
  Yosys_atd_j.read_t st buf
;;

let from_channel channel =
  let st = Yojson.init_lexer () in
  let buf = Lexing.from_channel channel in
  Yosys_atd_j.read_t st buf
;;

let from_file filename =
  let channel = In_channel.create filename in
  let t = from_channel channel in
  In_channel.close channel;
  t
;;
