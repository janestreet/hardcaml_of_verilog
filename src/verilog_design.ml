open Base

module Parameter = struct
  type t = Hardcaml.Parameter.t [@@deriving equal ~localize]

  type simple_parameter = Hardcaml.Parameter_name.t * Hardcaml.Parameter.Value.t
  [@@deriving sexp]

  let sexp_of_t (t : Hardcaml.Parameter.t) = sexp_of_simple_parameter (t.name, t.value)

  let t_of_sexp s =
    let name, value = simple_parameter_of_sexp s in
    { Hardcaml.Parameter.name; value }
  ;;

  let create = Hardcaml.Parameter.create
  let name { Hardcaml.Parameter.name; value = _ } = Hardcaml.Parameter_name.to_string name
  let value { Hardcaml.Parameter.name = _; value } = value

  let string_of_value { Hardcaml.Parameter.name = _; value } =
    match value with
    | Int i -> Int.to_string i
    | String s -> "\"" ^ s ^ "\""
    | _ ->
      raise_s [%message "Invalid parameter type" (value : Hardcaml.Parameter.Value.t)]
  ;;
end

module Parameters = struct
  type t = Parameter.t list [@@deriving sexp, equal ~localize]

  let rec replace (t : t) (parameter : Parameter.t) =
    match t with
    | [] -> []
    | hd :: tl ->
      if Hardcaml.Parameter_name.equal hd.name parameter.name
      then parameter :: tl
      else hd :: replace tl parameter
  ;;

  let replace t ~with_ = List.fold with_ ~init:t ~f:(fun ps p -> replace ps p)
end

module Define_value = struct
  type t =
    | String of string
    | Int of int
    | No_arg
  [@@deriving sexp, equal ~localize]

  let to_string = function
    | Int i -> Int.to_string i
    | String s -> (* strings are not quoted in defines *) s
    | No_arg -> raise_s [%message "Cannot convert [Define_value.No_arg] to string"]
  ;;
end

module Define = struct
  type t =
    { name : string
    ; value : Define_value.t
    }
  [@@deriving equal ~localize, fields ~getters]

  type simple_define = string * Define_value.t [@@deriving sexp]

  let sexp_of_t (t : t) = sexp_of_simple_define (t.name, t.value)

  let t_of_sexp s =
    let name, value = simple_define_of_sexp s in
    { name; value }
  ;;

  let create ~name ~value = { name; value }
end

module Defines = struct
  type t = Define.t list [@@deriving sexp, equal ~localize]
end

module Path = struct
  type t = string [@@deriving sexp, equal ~localize]
end

module Module = struct
  type t =
    { module_name : string
    ; path : Path.t
    ; instantiates : t list [@sexp.default []]
    ; parameters : Parameters.t [@sexp.default []]
    ; blackbox : bool [@sexp.default false]
    }
  [@@deriving sexp, fields ~getters]

  let create
    ?(blackbox = false)
    ?(parameters = [])
    ?(instantiates = [])
    ~module_name
    ~path
    ()
    =
    { module_name; path; instantiates; parameters; blackbox }
  ;;

  let override ?module_name ?path ?instantiates ?parameters ?blackbox t =
    let module_name = Option.value module_name ~default:t.module_name in
    let path = Option.value path ~default:t.path in
    let instantiates = Option.value instantiates ~default:t.instantiates in
    let parameters = Option.value parameters ~default:t.parameters in
    let blackbox = Option.value blackbox ~default:t.blackbox in
    create ~module_name ~path ~instantiates ~parameters ~blackbox ()
  ;;

  let rec iter t ~f =
    List.iter (instantiates t) ~f:(fun t -> iter t ~f);
    f t
  ;;

  let rec map t ~f =
    f { t with instantiates = List.map (instantiates t) ~f:(fun t -> map t ~f) }
  ;;

  let rec flat_map t ~f =
    let x = List.map (instantiates t) ~f:(fun t -> flat_map t ~f) |> List.concat in
    f t :: x
  ;;
end

type t =
  { top : Module.t
  ; defines : Defines.t [@sexp.default []]
  }
[@@deriving sexp, fields ~getters]

let create ?(defines = []) ~top () = { top; defines }
let top_name t = t.top.module_name
let override_parameters t parameters = { t with top = Module.override ~parameters t.top }

let map_paths t ~f =
  { t with
    top = Module.map t.top ~f:(fun m -> Module.override ~path:(f (Module.path m)) m)
  }
;;

module type Crunched = sig
  val read : string -> string option
end

let find_in_crunched crunched path =
  List.find_map crunched ~f:(fun (module Crunched : Crunched) -> Crunched.read path)
  |> Option.value_exn
       ~error:
         (Error.create_s [%message "Unable to extract crunched file" (path : string)])
;;

let map_crunched_paths ?(delete_temp_files = true) crunched t =
  let seen = Hashtbl.create (module String) in
  map_paths t ~f:(fun path ->
    match Hashtbl.find seen path with
    | Some path -> path
    | None ->
      let tmp_file = Filename_unix.temp_file "crunched" ".v" in
      if delete_temp_files then Stdlib.at_exit (fun () -> Unix.unlink tmp_file);
      let data = find_in_crunched crunched path in
      Stdio.Out_channel.write_all tmp_file ~data;
      Hashtbl.set seen ~key:path ~data:tmp_file;
      tmp_file)
;;

module type Embedded_files = sig
  val by_filename : (string * string) list
end

let find_in_embedded_files embedded_files path =
  (* embed file strips any leading path out. *)
  let file = Stdlib.Filename.basename path in
  match
    List.find_map embedded_files ~f:(fun (module Embedded_files : Embedded_files) ->
      List.Assoc.find Embedded_files.by_filename file ~equal:String.equal)
  with
  | None -> raise_s [%message "Unable to extract crunched file" (path : string)]
  | Some data -> data
;;

let map_embed_file_paths ?(delete_temp_files = true) embedded_files t =
  let seen = Hashtbl.create (module String) in
  map_paths t ~f:(fun path ->
    match Hashtbl.find seen path with
    | Some path -> path
    | None ->
      let tmp_file = Filename_unix.temp_file "crunched" ".v" in
      if delete_temp_files then Stdlib.at_exit (fun () -> Unix.unlink tmp_file);
      let data = find_in_embedded_files embedded_files path in
      Stdio.Out_channel.write_all tmp_file ~data;
      Hashtbl.set seen ~key:path ~data:tmp_file;
      tmp_file)
;;
