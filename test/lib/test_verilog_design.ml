open Core
module V = Hardcaml_of_verilog.Verilog_design
module M = V.Module

let top =
  V.create
    ~top:
      (M.create
         ~module_name:"foo"
         ~path:"vlog/foo.v"
         ~parameters:
           [ V.Parameter.create ~name:"A" ~value:(Int 1)
           ; V.Parameter.create ~name:"B" ~value:(String "popcorn")
           ]
         ~instantiates:
           [ M.create ~module_name:"bar" ~path:"vlog/bar.v" ~blackbox:true ()
           ; M.create
               ~module_name:"fudge"
               ~path:"vlog/fudge.v"
               ~instantiates:[ M.create ~module_name:"brumble" ~path:"vlog/brumble.v" () ]
               ()
           ]
         ())
    ()
;;

let%expect_test "map/iter" =
  let modules = V.top top in
  M.iter modules ~f:(fun m -> Out_channel.print_endline (M.module_name m));
  [%expect
    {|
    bar
    brumble
    fudge
    foo
    |}];
  let modules =
    M.map modules ~f:(fun m ->
      M.create
        ~module_name:(M.module_name m |> String.uppercase)
        ~path:(M.path m)
        ~instantiates:(M.instantiates m)
        ())
  in
  print_s [%message (modules : M.t)];
  [%expect
    {|
    (modules
     ((module_name FOO) (path vlog/foo.v)
      (instantiates
       (((module_name BAR) (path vlog/bar.v) (instantiates ()) (parameters ())
         (blackbox false))
        ((module_name FUDGE) (path vlog/fudge.v)
         (instantiates
          (((module_name BRUMBLE) (path vlog/brumble.v) (instantiates ())
            (parameters ()) (blackbox false))))
         (parameters ()) (blackbox false))))
      (parameters ()) (blackbox false)))
    |}];
  let modules = M.flat_map modules ~f:(fun m -> M.module_name m, M.path m) in
  print_s [%message (modules : (string * string) list)];
  [%expect
    {|
    (modules
     ((FOO vlog/foo.v) (BAR vlog/bar.v) (FUDGE vlog/fudge.v)
      (BRUMBLE vlog/brumble.v)))
    |}]
;;
