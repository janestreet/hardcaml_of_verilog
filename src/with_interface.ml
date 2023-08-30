open Base

module Make (I : Hardcaml.Interface.S) (O : Hardcaml.Interface.S) = struct
  let inputs (i : _ I.t) =
    I.map2 I.port_names i ~f:(fun name signal ->
      { Verilog_circuit.Port.name; value = signal })
    |> I.to_list
  ;;

  let outputs (o : _ Verilog_circuit.Port.t list) =
    Or_error.try_with (fun () ->
      O.map O.port_names ~f:(fun port -> (Verilog_circuit.Port.find_exn o port).value))
  ;;

  let create ?verbose ?passes verilog_design =
    let%bind.Or_error netlist = Netlist.create ?verbose ?passes verilog_design in
    let%bind.Or_error circuit =
      Verilog_circuit.create netlist ~top_name:(Verilog_design.top_name verilog_design)
    in
    let create_fn = Verilog_circuit.create_fn circuit in
    Ok
      (fun i ->
        let inputs = inputs i in
        let%bind.Or_error o = create_fn inputs in
        outputs o)
  ;;
end
