include Base
module Filename = Caml.Filename
module In_channel = Stdio.In_channel
module Out_channel = Stdio.Out_channel
module Parameter = Hardcaml.Parameter
module Parameter_name = Hardcaml.Parameter_name
module Unix = Core_unix

let fprintf = Out_channel.fprintf
let printf = Stdio.printf
let sprintf = Printf.sprintf
