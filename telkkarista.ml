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
    Arg.(value & opt string (Option.default "" (Persist.get_session env.Common.e_persist)) & info ["s"; "session"] ~docs ~doc)
  in
  Term.(pure (common_opts env) $ session)

let username_t persist =
  let doc = "User name (email). This is required." in
  Arg.(required & opt (some string) (Option.map fst (Persist.get_email_password persist)) & info ["u"; "user"] ~doc)

let password_t persist =
  let doc = "Password. This is required." in
  Arg.(required & opt (some string) (Option.map snd (Persist.get_email_password persist)) & info ["p"; "pass"] ~doc)

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
  Arg.(required & opt (some datetime) None & info ["b"; "begin"] ~doc)

let end_t =
  let doc = "End time stamp of the range." in
  Arg.(required & opt (some datetime) None & info ["e"; "end"] ~doc)

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

let cmd_list env =
  let range common from_ to_ =
    interactive_request Endpoints.range_request common.Common.c_session { API.from_; to_ } @@
    fun response -> API.show_range_response response
  in
  let doc = "List programs from given time range" in
  Term.(pure (lwt3 range) $ common_opts_t env $ begin_t $ end_t),
  Term.info "list" ~doc

let main () =
  let subcommands = [
    cmd_checkSession;
    cmd_login;
    cmd_list;
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
