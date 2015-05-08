open Batteries

type t = {
  t_toml : Toml.Value.table ref;
  t_changed : bool ref;
}

let config_dir_base = "telkkarista"

let (^/) a b = a ^ "/" ^ b

let config_base =
  let home =
    try Sys.getenv "HOME"
    with Not_found -> failwith "HOME environment variable is not set"
  in
  let has_dot_config = Sys.is_directory @@ home ^/ ".config" in
  let base =
    (if has_dot_config
     then home ^/ ".config"
     else home) ^/ config_dir_base
  in
  if not (Sys.file_exists base) then
    Unix.mkdir base 0o750;
  base

let persist_file = config_base ^/ "session"

let load_persist () =
  let toml =
    if Sys.file_exists persist_file
    then Toml.Parser.from_filename persist_file
    else Toml.Table.empty
  in
  { t_toml = ref toml;
    t_changed = ref false; }

let save_persist t =
  if !(t.t_changed) then
    BatFile.with_file_out ~mode:[`create; `trunc] ~perm:(BatFile.unix_perm 0o640) persist_file @@ fun file ->
    let fmt = Format.formatter_of_out_channel file in
    Toml.Printer.table fmt !(t.t_toml)

let set_string t key value =
  t.t_toml := Toml.Table.add (Toml.key key) (Toml.Value.Of.string value) !(t.t_toml);
  t.t_changed := true

let get_string t key =
  try Some (Toml.get_string (Toml.key key) !(t.t_toml))
  with Not_found -> None

let set_email_password t user pass =
  set_string t "username" user;
  set_string t "password" pass

let set_session t session =
  set_string t "session" session

let get_email_password t =
  match get_string t "username", get_string t "password" with
  | Some username, Some password -> Some (username, password)
  | _ -> None

let get_session t = get_string t "session"

let set_cache_server t cache_server = set_string t "cache_server" cache_server

let get_cache_server t = get_string t "cache_server"
