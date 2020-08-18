open! Import

let yosys_exe = Config.yosys

let with_args ?(verbose = true) () ~args =
  String.concat
    ~sep:" "
    [ Config.env; yosys_exe; args; (if verbose then "" else " >/dev/null") ]
;;

let with_script ?verbose () ~script_file =
  with_args () ?verbose ~args:("-s " ^ script_file)
;;
