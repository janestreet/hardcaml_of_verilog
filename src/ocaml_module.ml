open! Base
open Stdio
open Printf

module Rebuild_interfaces
    (I : Hardcaml.Interface.S)
    (O : Hardcaml.Interface.S)
    (X : sig
       val verilog_design : Verilog_design.t
       val loaded_design : Verilog_circuit.t
     end) =
struct
  let verilog_design = X.verilog_design

  let t_i, t_o, fn =
    ( Verilog_circuit.inputs X.loaded_design
    , Verilog_circuit.outputs X.loaded_design
    , Verilog_circuit.create_fn X.loaded_design )
  ;;

  module I = struct
    module T = struct
      include I

      let port_names_and_widths =
        map port_names ~f:(fun n -> n, (Verilog_circuit.Port.find_exn t_i n).value)
      ;;
    end

    include T
    include Hardcaml.Interface.Make (T)
  end

  module O = struct
    module T = struct
      include O

      let port_names_and_widths =
        map port_names ~f:(fun n -> n, (Verilog_circuit.Port.find_exn t_o n).value)
      ;;
    end

    include O
    include Hardcaml.Interface.Make (T)
  end

  let create i =
    let i =
      I.(
        to_list
        @@ map2 port_names i ~f:(fun n i -> Verilog_circuit.Port.{ name = n; value = i }))
    in
    let o = fn i |> Or_error.ok_exn in
    O.(map port_names ~f:(fun n -> (Verilog_circuit.Port.find_exn o n).value))
  ;;

  let inst ?(name = Verilog_design.top_name verilog_design) ?instance i =
    let module Inst = Hardcaml.Instantiation.With_interface (I) (O) in
    Inst.create ?instance ~name i
  ;;

  let hierarchical ?(name = Verilog_design.top_name verilog_design) ?instance scope =
    let module Hier = Hardcaml.Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ?instance ~name (fun _scope -> create)
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
    {ocaml_module|open Base

let verilog_design =
  Hardcaml_of_verilog.Verilog_design.t_of_sexp
   (Parsexp.Single.parse_string_exn {|
%{verilog_design}
|})

let name =
  Hardcaml_of_verilog.Verilog_design.Module.module_name
    (Hardcaml_of_verilog.Verilog_design.top verilog_design)

module type P = sig
%{module_type_params}
end

module P = struct
%{module_params}
end

module I = struct
  type 'a t = {
%{input_fields}
  }[@@deriving hardcaml ~rtlmangle:false]
end

module O = struct
  type 'a t = {
%{output_fields}
  }[@@deriving hardcaml ~rtlmangle:false]
end

module From_verilog(P : P)(X : sig
  val verbose : bool
  val map_verilog_design
    :  Hardcaml_of_verilog.Verilog_design.t
    -> Hardcaml_of_verilog.Verilog_design.t
end) = struct
  let params = [
%{instantiated_params}
  ]

  include Hardcaml_of_verilog.Ocaml_module.Rebuild_interfaces(I)(O)(struct
    let verilog_design =
      Hardcaml_of_verilog.Verilog_design.override_parameters
        (X.map_verilog_design verilog_design) params

    let loaded_design =
      let create () =
        let%bind.Or_error netlist =
          Hardcaml_of_verilog.Netlist.create ~verbose:X.verbose verilog_design
        in
        Hardcaml_of_verilog.Verilog_circuit.create
          netlist
          ~top_name:
            (Hardcaml_of_verilog.Verilog_design.Module.module_name
              (Hardcaml_of_verilog.Verilog_design.top verilog_design))
      in
      create () |> Or_error.ok_exn
  end)
end

module From_json(X : sig val json : string end) = struct
  include Hardcaml_of_verilog.Ocaml_module.Rebuild_interfaces(I)(O)(struct
    let verilog_design = verilog_design

    let loaded_design =
      let create () =
        let%bind.Or_error yosys_netlist = Hardcaml_of_verilog.Expert.Yosys_netlist.of_string X.json in
        let%bind.Or_error netlist = Hardcaml_of_verilog.Netlist.of_yosys_netlist yosys_netlist in
        Hardcaml_of_verilog.Verilog_circuit.create
          netlist
          ~top_name:
            (Hardcaml_of_verilog.Verilog_design.Module.module_name
              (Hardcaml_of_verilog.Verilog_design.top verilog_design))
      in
      create () |> Or_error.ok_exn
    end)
end
|ocaml_module}]
;;

let to_ocaml verilog_design loaded_design =
  let open Verilog_design in
  let mapping = [ "match", "match_"; "type", "type_" ] in
  let oname n =
    let n = String.lowercase n in
    match List.Assoc.find ~equal:String.equal mapping n with
    | Some n -> n
    | None -> n
  in
  let field (p : _ Verilog_circuit.Port.t) =
    sprintf "    %s : 'a [@rtlname \"%s\"];" (oname p.name) p.name
  in
  let param_type p =
    let name = Parameter.name p in
    match Parameter.value p with
    | Int _ -> sprintf "  val %s : int" (oname name)
    | String _ -> sprintf "  val %s : string" (oname name)
    | v ->
      raise_s [%message "Unsupported parameter type" (v : Hardcaml.Parameter.Value.t)]
  in
  let param_value p =
    let name = Parameter.name p in
    match Parameter.value p with
    | Int x -> sprintf "  let %s = %i" (oname name) x
    | String x -> sprintf "  let %s = \"%s\"" (oname name) x
    | v ->
      raise_s [%message "Unsupported parameter type" (v : Hardcaml.Parameter.Value.t)]
  in
  let param_spec p =
    let name = Parameter.name p in
    let value = Parameter.value p in
    let value =
      match value with
      | Int _ -> "Int"
      | String _ -> "String"
      | v ->
        raise_s [%message "Unsupported parameter type" (v : Hardcaml.Parameter.Value.t)]
    in
    String.concat
      [ "    Hardcaml.Parameter.create"
      ; " ~name:\""
      ; name
      ; "\""
      ; " ~value:("
      ; value
      ; " P."
      ; oname name
      ; ");"
      ]
  in
  let lines l f = String.concat ~sep:"\n" (List.map ~f l) in
  let inputs = Verilog_circuit.inputs loaded_design in
  let outputs = Verilog_circuit.outputs loaded_design in
  let parameters = Verilog_design.(top verilog_design |> Module.parameters) in
  template
    ~verilog_design:(Sexp.to_string_hum (Verilog_design.sexp_of_t verilog_design))
    ~module_type_params:(lines parameters param_type)
    ~module_params:(lines parameters param_value)
    ~input_fields:(lines inputs field)
    ~output_fields:(lines outputs field)
    ~instantiated_params:(lines parameters param_spec)
;;

let save_ocaml verilog_design loaded_design ~file =
  Out_channel.write_all file ~data:(to_ocaml verilog_design loaded_design)
;;
