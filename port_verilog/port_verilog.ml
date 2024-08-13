open Core
open Hardcaml
open Hardcaml_of_verilog

module Make (X : sig
    val verilog_design : Verilog_design.t
    val hardcaml_circuit : Circuit.t
  end) =
struct
  module Verilog () = struct
    let netlist = Netlist.create X.verilog_design |> Or_error.ok_exn

    let verilog_circuit =
      Verilog_circuit.create netlist ~top_name:(Verilog_design.top_name X.verilog_design)
      |> Or_error.ok_exn
    ;;

    let circuit = Verilog_circuit.to_hardcaml_circuit verilog_circuit |> Or_error.ok_exn
  end

  module Verify () = struct
    module Verilog = Verilog ()

    let verify ~instantiation_propositions ~register_propositions () =
      let hardcaml_circuit = X.hardcaml_circuit in
      let verilog_circuit = Verilog.circuit in
      let%bind.Or_error t =
        Hardcaml_verify.Sec.create
          ~instantiation_ports_match:Left_is_subset_of_right
          verilog_circuit
          hardcaml_circuit
      in
      let propositions =
        List.map instantiation_propositions ~f:(fun name ->
          Hardcaml_verify.Sec.find_instantiation_inputs_proposition t ~name
          |> Option.value_exn)
        @ List.map register_propositions ~f:(fun name ->
          Hardcaml_verify.Sec.find_register_inputs_proposition t ~name |> Option.value_exn)
      in
      match propositions with
      | [] -> Hardcaml_verify.Sec.circuits_equivalent t
      | _ -> Hardcaml_verify.Sec.equivalent propositions
    ;;

    let find_instantiation_ports instantiation_name =
      let open Hardcaml_of_verilog in
      let%bind.Or_error module_ =
        Netlist.find_module_by_name
          Verilog.netlist
          (Verilog_design.top_name X.verilog_design)
      in
      match
        List.find module_.cells ~f:(fun cell ->
          String.equal cell.instance_name instantiation_name)
      with
      | None -> Ok []
      | Some cell -> Ok (List.map cell.inputs ~f:(fun port -> port.name))
    ;;

    type port_result =
      | Qed
      | Different
      | No_port
      | Error
    [@@deriving sexp_of]

    let verify_instantiation_ports t ~instantiation_name ~port_name =
      let%bind.Or_error port_names =
        match port_name with
        | None -> find_instantiation_ports instantiation_name
        | Some port_name -> Ok [ port_name ]
      in
      Ok
        (List.map port_names ~f:(fun port_name ->
           ( port_name
           , match
               Hardcaml_verify.Sec.find_instantiation_input_port_proposition
                 t
                 ~instantiation_name
                 ~port_name
             with
             | None -> No_port
             | Some proposition ->
               (match Hardcaml_verify.Sec.equivalent [ proposition ] with
                | Ok Unsat -> Qed
                | Ok (Sat _) -> Different
                | Error _ -> Error) )))
    ;;

    let verify_top_ports t ~port_name =
      let open Hardcaml_of_verilog in
      let%bind.Or_error module_ =
        Netlist.find_module_by_name
          Verilog.netlist
          (Verilog_design.top_name X.verilog_design)
      in
      let port_names =
        match port_name with
        | None -> List.map module_.outputs ~f:(fun o -> o.name)
        | Some port_name -> [ port_name ]
      in
      Ok
        (List.map port_names ~f:(fun port_name ->
           ( port_name
           , match
               Hardcaml_verify.Sec.find_circuit_output_port_proposition t ~port_name
             with
             | None -> No_port
             | Some proposition ->
               (match Hardcaml_verify.Sec.equivalent [ proposition ] with
                | Ok Unsat -> Qed
                | Ok (Sat _) -> Different
                | Error _ -> Error) )))
    ;;
  end

  let command_verify =
    Command.basic
      ~summary:"Perform verification to show equivalence"
      [%map_open.Command
        let () = return ()
        and instantiation_propositions =
          flag
            "-instantiation"
            (listed string)
            ~doc:"PROP only check these instantiations"
        and register_propositions =
          flag "-register" (listed string) ~doc:"PROP only check these registers"
        in
        fun () ->
          let module Verify = Verify () in
          let result =
            Verify.verify ~instantiation_propositions ~register_propositions ()
          in
          print_s
            [%message (result : Hardcaml_verify.Sec.Equivalence_result.t Or_error.t)]]
  ;;

  let command_verify_ports =
    Command.basic
      ~summary:""
      [%map_open.Command
        let () = return ()
        and instantiation_name = flag "-instantiation" (optional string) ~doc:""
        and port_name = flag "-port" (optional string) ~doc:"" in
        fun () ->
          let module Verify = Verify () in
          let verify () =
            let hardcaml_circuit = X.hardcaml_circuit in
            let verilog_circuit = Verify.Verilog.circuit in
            let%bind.Or_error t =
              Hardcaml_verify.Sec.create
                ~instantiation_ports_match:Left_is_subset_of_right
                verilog_circuit
                hardcaml_circuit
            in
            match instantiation_name with
            | None -> Verify.verify_top_ports t ~port_name
            | Some instantiation_name ->
              Verify.verify_instantiation_ports t ~instantiation_name ~port_name
          in
          let result = verify () in
          print_s [%message (result : (string * Verify.port_result) list Or_error.t)]]
  ;;

  let command_print_verilog_netlist =
    Command.basic
      ~summary:"Print rtl as a netlist"
      [%map_open.Command
        let () = return () in
        fun () ->
          let module Verilog = Verilog () in
          let netlist () = Netlist.get_all_modules Verilog.netlist in
          print_s [%message (netlist () : Netlist.Module.t list Or_error.t)]]
  ;;

  let command_print_verilog_rtl =
    Command.basic
      ~summary:"Print verilog design as rtl"
      [%map_open.Command
        let () = return () in
        fun () ->
          let module Verilog = Verilog () in
          Rtl.print Verilog Verilog.circuit]
  ;;

  let command_list_verilog_ports =
    Command.basic
      ~summary:"Show the ports of the top level verilog design"
      [%map_open.Command
        let () = return () in
        fun () ->
          let module Verilog = Verilog () in
          let circuit = Verilog.circuit in
          let get_ports =
            List.map ~f:(fun signal ->
              Signal.names signal |> List.hd_exn, Signal.width signal)
          in
          let inputs = Circuit.inputs circuit |> get_ports in
          let outputs = Circuit.outputs circuit |> get_ports in
          print_s
            [%message (inputs : (string * int) list) (outputs : (string * int) list)]]
  ;;

  let command_list_verilog_instantiations =
    Command.basic
      ~summary:"List all instantiations in verilog netlist"
      [%map_open.Command
        let () = return () in
        fun () ->
          let module Verilog = Verilog () in
          let netlist = Verilog.netlist in
          let top =
            Netlist.find_module_by_name netlist (Verilog_design.top_name X.verilog_design)
            |> Or_error.ok_exn
          in
          let instantiation_names =
            List.map top.cells ~f:(fun cell -> cell.instance_name)
          in
          print_s [%message (instantiation_names : string list)]]
  ;;

  let command_print_hardcaml_rtl =
    Command.basic
      ~summary:"Print hardcaml circuit as verilog"
      [%map_open.Command
        let () = return () in
        fun () ->
          let circuit = X.hardcaml_circuit in
          Rtl.print Verilog circuit]
  ;;

  let command_list_hardcaml_ports =
    Command.basic
      ~summary:"Show the ports of the top level design"
      [%map_open.Command
        let () = return () in
        fun () ->
          let inputs =
            Circuit.inputs X.hardcaml_circuit
            |> List.map ~f:(fun s -> Signal.names s |> List.hd_exn, Signal.width s)
          in
          let outputs =
            Circuit.outputs X.hardcaml_circuit
            |> List.map ~f:(fun s -> Signal.names s |> List.hd_exn, Signal.width s)
          in
          print_s
            [%message (inputs : (string * int) list) (outputs : (string * int) list)]]
  ;;

  let command =
    Command.group
      ~summary:""
      [ ( "verilog-design"
        , Command.group
            ~summary:"operations on the verilog rtl design"
            [ "netlist", command_print_verilog_netlist
            ; "verilog", command_print_verilog_rtl
            ; "ports", command_list_verilog_ports
            ; "instantiations", command_list_verilog_instantiations
            ] )
      ; ( "hardcaml"
        , Command.group
            ~summary:"operations on the hardcaml design"
            [ "verilog", command_print_hardcaml_rtl
            ; "ports", command_list_hardcaml_ports
            ] )
      ; ( "verify"
        , Command.group
            ~summary:"verification"
            [ "design", command_verify; "ports", command_verify_ports ] )
      ]
  ;;
end
