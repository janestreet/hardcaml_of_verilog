open! Base
open Stdio
open Printf
open Hardcaml

module Rebuild_interfaces
    (I : Interface.S)
    (O : Interface.S) (X : sig
                         val loaded_design : Synthesized_design.t
                       end) =
struct
  let t_i, t_o, fn =
    ( Synthesized_design.inputs X.loaded_design
    , Synthesized_design.outputs X.loaded_design
    , Synthesized_design.create_fn X.loaded_design )
  ;;

  module I = struct
    module T = struct
      include I

      let t = map ~f:(fun (n, _) -> n, Caml.List.assoc n t_i) t
    end

    include T
    include Interface.Make (T)
  end

  module O = struct
    module T = struct
      include O

      let t = map ~f:(fun (n, _) -> n, Caml.List.assoc n t_o) t
    end

    include O
    include Interface.Make (T)
  end

  let create i =
    let i = I.(to_list @@ map2 ~f:(fun (n, _) i -> n, i) t i) in
    let o = fn i in
    O.(map ~f:(fun (n, _) -> Caml.List.assoc n o) t)
  ;;
end

let template
      ~verilog_design
      ~module_type_params
      ~module_params
      ~input_fields
      ~output_fields
      ~instantiated_params
  =
  [%string
    {ocaml_module|
let verilog_design =
  Hardcaml_of_verilog.Verilog_design.t_of_sexp
   (Parsexp.Single.parse_string_exn {|
%{verilog_design}
|})

let name = verilog_design.name

module type P = sig
%{module_type_params}
end

module P = struct
%{module_params}
end

module I = struct
  type 'a t = {
%{input_fields}
  }[@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = {
%{output_fields}
  }[@@deriving sexp_of, hardcaml]
end

module From_verilog(P : P)(X : sig
  val blackbox : bool
  val file_io : Hardcaml_of_verilog.Verilog_design.File_io.t option
end) = struct
  let params = [
%{instantiated_params}
  ]

  include Hardcaml_of_verilog.Ocaml_module.Rebuild_interfaces(I)(O)(struct
    let loaded_design =
      Hardcaml_of_verilog.Verilog_design.load
        ?file_io:X.file_io
        ~blackbox:X.blackbox
        { verilog_design with params }
  end)
end

module From_json(X : sig val json : string end) = struct
  include Hardcaml_of_verilog.Ocaml_module.Rebuild_interfaces(I)(O)(struct
    let loaded_design =
      Hardcaml_of_verilog.Verilog_design.of_json
        ~topname:verilog_design.name X.json |> snd
    end)
end
|ocaml_module}]
;;

let to_ocaml verilog_design loaded_design =
  let open Verilog_design in
  let mapping = [ "match", "match_"; "type", "type_" ] in
  let oname n =
    let n = String.lowercase (n |> Parameter_name.to_string) in
    match List.Assoc.find ~equal:String.equal mapping n with
    | Some n -> n
    | None -> n
  in
  let field (n, _) =
    sprintf "    %s : 'a [@rtlname \"%s\"];" (oname (n |> Parameter_name.of_string)) n
  in
  let open Parameter in
  let param_type = function
    | { name; value = Int _ } -> sprintf "  val %s : int" (oname name)
    | { name; value = String _ } -> sprintf "  val %s : string" (oname name)
    | value -> raise_s [%message "Unsupported parameter type" (value : Parameter.t)]
  in
  let param_value = function
    | { name; value = Int x } -> sprintf "  let %s = %i" (oname name) x
    | { name; value = String x } -> sprintf "  let %s = \"%s\"" (oname name) x
    | value -> raise_s [%message "Unsupported parameter type" (value : Parameter.t)]
  in
  let param_spec { name; value } =
    let value =
      match value with
      | Int _ -> "Int"
      | String _ -> "String"
      | value ->
        raise_s [%message "Unsupported parameter type" (value : Parameter.Value.t)]
    in
    String.concat
      [ "    Hardcaml.Parameter.create"
      ; " ~name:\""
      ; name |> Parameter_name.to_string
      ; "\""
      ; " ~value:("
      ; value
      ; " P."
      ; oname name
      ; ");"
      ]
  in
  let lines l f = String.concat ~sep:"\n" (List.map ~f l) in
  let _name = Synthesized_design.name loaded_design in
  let inputs = Synthesized_design.inputs loaded_design in
  let outputs = Synthesized_design.outputs loaded_design in
  template
    ~verilog_design:(Sexp.to_string_hum (Verilog_design.sexp_of_t verilog_design))
    ~module_type_params:(lines verilog_design.params param_type)
    ~module_params:(lines verilog_design.params param_value)
    ~input_fields:(lines inputs field)
    ~output_fields:(lines outputs field)
    ~instantiated_params:(lines verilog_design.params param_spec)
;;

let save_ocaml verilog_design loaded_design ~file =
  Out_channel.write_all file ~data:(to_ocaml verilog_design loaded_design)
;;
