open Batteries
open Lwt.Infix
open Cmdliner

let program_name = "telkkarista"
let version = "0.1"

let return = Lwt.return

let interactive_request endpoint session arg show =
  let%lwt response = endpoint session arg in
  match response with
  | None ->
    Printf.printf "There was an error\n%!";
    return ()
  | Some response ->
    Printf.printf "%s\n%!" (show response);
    return ()

let common_opts env session = {
  Common.c_session = session;
  c_env = env;
}

let common_opts_t env =
  let docs = "COMMON OPTIONS" in 
  let session = 
    let doc = "Set session id" in
    Arg.(value & opt string (Option.default "" (Persist.get_session env.Common.e_persist)) & info ["s"; "session"] ~docv:"TOKEN" ~docs ~doc)
  in
  Term.(pure (common_opts env) $ session)

let req_optional kind default info_ =
  match default with
  | None -> Arg.(required & opt (some kind) None & info_)
  | Some default -> Arg.(value & opt kind default & info_)

let username_t persist =
  let doc = "User name (email). This is required." in
  req_optional Arg.string (Option.map fst (Persist.get_email_password persist)) (Arg.info ["u"; "user"] ~docv:"EMAIL" ~doc)

let password_t persist =
  let doc = "Password. This is required." in
  req_optional Arg.string (Option.map snd (Persist.get_email_password persist)) (Arg.info ["p"; "pass"] ~docv:"PASSWORD" ~doc)

let pid_t =
  let doc = "Program ID. This is required." in
  req_optional Arg.string None (Arg.info ["p"; "pid"] ~docv:"PID" ~doc)

let format_t =
  let doc = "Format for download. This is required." in
  req_optional Arg.string None (Arg.info ["f"; "format"] ~docv:"FORMAT" ~doc)

let quality_t =
  let doc = "Quality for download. This is required." in
  req_optional Arg.string None (Arg.info ["q"; "quality"] ~docv:"QUALITY" ~doc)

let best_cache persist =
  match
    Persist.get_cache_servers persist |>
    List.sort (fun (_, a) (_, b) -> compare b.API.mbit a.API.mbit)
  with
  | (fastest, _)::_ -> Some fastest
  | _ -> None

let cache_server_t persist =
  let doc = "Cache server for download. This is required." in
  req_optional Arg.string (best_cache persist) (Arg.info ["c"; "cache"] ~docv:"SERVER" ~doc)

let datetime : float Cmdliner.Arg.converter =
  let parser str =
    try
      ( match ISO8601.Permissive.datetime_tz str with
        | (t, Some tz) -> `Ok t
        | (t0, None) -> `Ok (t0 -. float (Netdate.get_localzone ()) *. 60.0))
    with _ -> `Error ("Invalid date time: " ^ str)
  in
  let printer fmt time = Format.fprintf fmt "%s" (ISO8601.Permissive.string_of_datetime time) in
  (parser, printer)

let begin_t =
  let doc = "Begin time stamp of the range." in
  Arg.(required & opt (some datetime) None & info ["b"; "begin"] ~docv:"ISO8601 TIME" ~doc)

let end_t =
  let doc = "End time stamp of the range." in
  Arg.(required & opt (some datetime) None & info ["e"; "end"] ~docv:"ISO8601 TIME" ~doc)

let help_subcommands = [
  `S "COMMON OPTIONS";
  `P "These options are common to all commands.";
  `S "MORE HELP";
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
  `S "BUGS"; `P "Check bug reports at http://bugs.example.org.";]

let default_prompt env = 
  let doc = "An unofficial standalone command line client for Telkkarista.com" in 
  let man = help_subcommands in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_opts_t env)),
  Term.info program_name ~version ~sdocs:"COMMON OPTIONS" ~doc ~man

let lwt1 f a = Lwt_unix.run (f a)
let lwt2 f a b = Lwt_unix.run (f a b)
let lwt3 f a b c = Lwt_unix.run (f a b c)
let lwt4 f a b c d = Lwt_unix.run (f a b c d)
let lwt5 f a b c d e = Lwt_unix.run (f a b c d e)

let cmd_checkSession env =
  let checkSession common =
    interactive_request Endpoints.checkSession_request common.Common.c_session () @@
    fun response -> response.API._id
  in
  let doc = "Check session status" in
  Term.(pure (lwt1 checkSession) $ common_opts_t env),
  Term.info "checkSession" ~doc

let cmd_login env =
  let login common email password =
    interactive_request Endpoints.login_request () { API.email; password } @@
    fun token ->
    Persist.set_email_password env.Common.e_persist email password;
    Persist.set_session env.Common.e_persist token;
    token
  in
  let doc = "Log into the service" in
  Term.(pure (lwt3 login) $ common_opts_t env $ username_t env.Common.e_persist $ password_t env.Common.e_persist),
  Term.info "login" ~doc

let cmd_settings env =
  let settings common =
    interactive_request Endpoints.user_settings_request common.Common.c_session () @@
    fun settings ->
    List.iter
      (function
        | API.Speedtests speedtests -> Persist.set_cache_servers env.Common.e_persist speedtests
        | Other _ -> ())
      settings;
    API.show_user_settings_response settings
  in
  let doc = "Retrieve settings" in
  Term.(pure (lwt1 settings) $ common_opts_t env),
  Term.info "settings" ~doc

let cmd_list env =
  let range common from_ to_ =
    interactive_request Endpoints.range_request common.Common.c_session { API.from_; to_ } @@
    fun response -> API.show_range_response response
  in
  let doc = "List programs from given time range" in
  Term.(pure (lwt3 range) $ common_opts_t env $ begin_t $ end_t),
  Term.info "list" ~doc

let cmd_cache env =
  let cache common =
    interactive_request Endpoints.cache_request common.Common.c_session () @@
    fun response -> API.show_cache_response response
  in
  let doc = "List cache servers" in
  Term.(pure (lwt1 cache) $ common_opts_t env),
  Term.info "cache" ~doc

(* useless currently. what are the parameters to the request? *)
let cmd_vod_url env =
  let vod_url common pid =
    match%lwt Endpoints.epg_info_request common.Common.c_session { API.pid } with
    | None ->
      Printf.printf "Sorry, no such programm id could be found\n";
      return ()
    | Some ({ recordpath = Some recordpath }) ->
      let request = {
        API.pid;
        path = recordpath;
        file = "highest.mp4";
      } in
      interactive_request Endpoints.client_vod_getUrl_request common.Common.c_session request @@
      fun response -> API.show_json_response response
    | Some _ ->
      Printf.printf "Sorry, no recording for the program id could be found\n";
      return ()
  in
  let doc = "NOT WORKING: Retrieve the URL of a program" in
  Term.(pure (lwt2 vod_url) $ common_opts_t env $ pid_t),
  Term.info "vod-url" ~doc

let cmd_url env =
  let url common cache_server pid format quality =
    match%lwt Endpoints.epg_info_request common.Common.c_session { API.pid } with
    | None ->
      Printf.printf "Sorry, no such programm id could be found\n";
      return ()
    | Some ({ recordpath = Some recordpath } as info) ->
      let url = Endpoints.download_url cache_server common.Common.c_session format quality info "file" in
      Printf.printf "%s\n%!" (Option.default "not available" url);
      return ()
    | Some _ ->
      Printf.printf "Sorry, no recording for the program id could be found\n";
      return ()
  in
  let doc = "Retrieve the URL of a program" in
  Term.(pure (lwt5 url) $ common_opts_t env $ cache_server_t env.Common.e_persist $ pid_t $ format_t $ quality_t),
  Term.info "url" ~doc
  

let cmd_epg_info env =
  let epg_info common pid =
    let request = {
      API.pid = pid;
    } in
    interactive_request Endpoints.epg_info_request common.Common.c_session request @@
    API.show_epg_info_response
  in
  let doc = "Retrieve info about a program" in
  Term.(pure (lwt2 epg_info) $ common_opts_t env $ pid_t),
  Term.info "info" ~doc

let main () =
  let subcommands = [
    cmd_checkSession;
    cmd_login;
    cmd_settings;
    cmd_list;
    cmd_cache;
    cmd_epg_info;
    cmd_vod_url;
    cmd_url;
  ] in
  let env = { Common.e_persist = Persist.load_persist () } in
  match Term.eval_choice (default_prompt env) (List.map (fun x -> x env) subcommands)
  with
  | `Error _ -> exit 1
  | _        ->
    Persist.save_persist env.Common.e_persist;
    exit 0

let _ =
  if not !(Sys.interactive) then
    main ()
