open! Import
open Hardcaml

module Make (I : Interface.S) (O : Interface.S) = struct
  let convert_to_interface design =
    Staged.stage (fun i ->
      let o =
        Synthesized_design.create_fn
          design
          (I.to_list @@ I.map2 ~f:(fun (n, _) i -> n, i) I.t i)
      in
      O.map ~f:(fun (n, _) -> List.Assoc.find_exn o n ~equal:String.equal) O.t)
  ;;

  let convert_exn ?use_netlist_names () ~blackboxes ~topname ~verilog =
    let json =
      Or_error.ok_exn
        (Synthesize.convert_to_json_netlist ~params:[] ~topname ~blackboxes ~verilog)
    in
    let designs =
      Or_error.ok_exn
        (Synthesized_designs.of_json_netlist
           json
           ~techlib:Techlib.Simlib.cells
           ?use_netlist_names)
    in
    convert_to_interface (Map.find_exn designs topname)
  ;;
end
