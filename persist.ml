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

let set_value t key value =
  t.t_toml := Toml.Table.add (Toml.key key) value !(t.t_toml);
  t.t_changed := true

let set_string t key value =
  set_value t key (Toml.Value.Of.string value)

let get_table t key =
  try Some (Toml.get_table (Toml.key key) !(t.t_toml))
  with Not_found -> None

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

let set_cache_servers t cache_servers =
  let (_, table) =
    List.fold_left
      (fun (count, table) (server, speedtest) ->
         let entry =
           Toml.Table.empty |>
           Toml.Table.add (Toml.key "host") (Toml.Value.Of.string server) |>
           Toml.Table.add (Toml.key "mbit") (Toml.Value.Of.float speedtest.API.mbit)
         in
         (count + 1, Toml.Table.add (Toml.key (Printf.sprintf "server%d" count)) (Toml.Value.Of.table entry) table)
      )
      (1, Toml.Table.empty)
      cache_servers
  in
  set_value t "caches" (Toml.Value.Of.table table)

let get_cache_servers t =
  match get_table t "caches" with
  | None -> []
  | Some caches ->
    (* TODO: not handling exceptions.. of course, we never read broken configuration! *)
    Toml.Table.fold
      (fun _ (info : Toml.Value.value) caches ->
         let host = Toml.get_string (Toml.key "host") (Toml.Value.To.table info) in
         let mbit = Toml.get_float (Toml.key "mbit") (Toml.Value.To.table info) in
         (host, { API.mbit = mbit; length = 0.0; })::caches
      )
      caches
      []
