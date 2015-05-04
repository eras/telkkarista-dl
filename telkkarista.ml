open Lwt.Infix
open Cmdliner

let program_name = "telkkarista"
let version = "0.1"
let base = "http://api.telkkarista.com"

let return = Lwt.return

let (/^) a b = a ^ "/" ^ b

type common = {
  c_session : string;
}

module API =
struct
  type 'a response = {
    status       : string;
    code         : string;
    payload      : 'a;
  } [@@deriving of_yojson]

  type checkSession_response = {
    _id          : string;
    user_id      : string;
    lastActivity : string;
    meta         : Yojson.Safe.json;
    email        : string;
  } [@@deriving of_yojson]

  type login_request = {
    email        : string;
    password     : string;
  } [@@deriving to_yojson]

  (* "status": "ok" *)
  (* "code": "login_ok" *)
  type login_response = string [@@deriving of_yojson]
end

module Endpoints =
struct
  let endpoint service = Uri.of_string (base /^ service)

  let login = endpoint "1/user/login"
      
  let checkSession = endpoint "1/user/checkSession"
end

let post ~headers ~endpoint ~body =
  let headers = Cohttp.Header.of_list (("Content-Length", Printf.sprintf "%d" (String.length body))::headers) in
  let%lwt (_, body) = Cohttp_lwt_unix.Client.post ~body:(`Stream (Lwt_stream.of_list [body])) ~headers endpoint in
  let%lwt body_string = Cohttp_lwt_body.to_string body in
  return (Yojson.Safe.from_string body_string)

let post_with_session ~session ~endpoint ~body = post ~headers:["X-SESSION", session] ~endpoint ~body

let post_without_session ~endpoint ~body = post ~headers:[] ~endpoint ~body

let get ~session ~endpoint =
  let headers = Cohttp.Header.of_list ["X-SESSION", session] in
  let%lwt (_, body) = Cohttp_lwt_unix.Client.get ~headers endpoint in
  let%lwt body_string = Cohttp_lwt_body.to_string body in
  Printf.printf "Response: %s\n%!" body_string;
  return (Yojson.Safe.from_string body_string)
    
let checkSession env common =
  Lwt_unix.run (
    (* ~body:(`Stream (Lwt_stream.of_list ["foo"])) *)
    let%lwt response = get ~session:common.c_session ~endpoint:Endpoints.checkSession in
    match API.response_of_yojson API.checkSession_response_of_yojson response with
    | `Error error ->
      Printf.printf "Failed to extract json: %s\n%!" error;
      return None;
      return ()
    | `Ok response ->
      Printf.printf "id: %s\n%!" response.API.payload._id;
      return (Some response);
      return ()
  )

let login env common email password =
  Lwt_unix.run (
    let body = Yojson.Safe.to_string @@ API.login_request_to_yojson { API.email; password } in
    let%lwt response = post_without_session ~endpoint:Endpoints.login ~body in
    match API.response_of_yojson API.login_response_of_yojson response with
    | `Error error ->
      Printf.printf "Failed to extract json: %s\n%!" error;
      return None;
      return ()
    | `Ok response ->
      Printf.printf "token: %s\n%!" response.API.payload;
      return (Some response);
      return ()
  )

let common_opts session = { c_session = session }

let common_opts_t =
  let docs = "COMMON OPTIONS" in 
  let session = 
    let doc = "Set session id" in
    Arg.(value & opt string "" & info ["s"; "session"] ~docs ~doc)
  in
  Term.(pure common_opts $ session)

let username_t =
  let doc = "User name (email)" in
  Arg.(value & opt string "" & info ["u"; "user"] ~doc)

let password_t =
  let doc = "Password" in
  Arg.(value & opt string "" & info ["p"; "pass"] ~doc)

let help_subcommands = [
  `S "COMMON OPTIONS";
  `P "These options are common to all commands.";
  `S "MORE HELP";
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;
  `P "Use `$(mname) help patterns' for help on patch matching."; `Noblank;
  `P "Use `$(mname) help environment' for help on environment variables.";
  `S "BUGS"; `P "Check bug reports at http://bugs.example.org.";]

let default_prompt = 
  let doc = "A standalone command line client for Telkkarista.com" in 
  let man = help_subcommands in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_opts_t)),
  Term.info program_name ~version ~sdocs:"COMMON OPTIONS" ~doc ~man

let cmd_checkSession env = 
  Term.(pure (checkSession env) $ common_opts_t),
  Term.info "checkSession" ~version

let cmd_login env = 
  Term.(pure (login env) $ common_opts_t $ username_t $ password_t),
  Term.info "login" ~version

let main () =
  match Term.eval_choice default_prompt [cmd_checkSession (); cmd_login ()] with `Error _ -> exit 1 | _ -> exit 0

let _ =
  if not !(Sys.interactive) then
    main ()
