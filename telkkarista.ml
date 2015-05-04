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
    payload      : 'a option [@default None];
  } [@@deriving of_yojson { strict = false }]

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
  let version = 1
  
  let endpoint service = Uri.of_string (base /^ Printf.sprintf "%d" version /^ service)

  let login = endpoint "user/login"
      
  let checkSession = endpoint "user/checkSession"
end

let session_header session = ["X-SESSION", session]

let post ~headers ~endpoint ~body =
  let headers = Cohttp.Header.of_list (("Content-Length", Printf.sprintf "%d" (String.length body))::headers) in
  let%lwt (_, body) = Cohttp_lwt_unix.Client.post ~body:(`Stream (Lwt_stream.of_list [body])) ~headers endpoint in
  let%lwt body_string = Cohttp_lwt_body.to_string body in
  return (Yojson.Safe.from_string body_string)

let post_with_session ~session ~endpoint ~body = post ~headers:(session_header session) ~endpoint ~body

let post_without_session ~endpoint ~body = post ~headers:[] ~endpoint ~body

let get ~session ~endpoint =
  let headers = Cohttp.Header.of_list (session_header session) in
  let%lwt (_, body) = Cohttp_lwt_unix.Client.get ~headers endpoint in
  let%lwt body_string = Cohttp_lwt_body.to_string body in
  Printf.printf "Response: %s\n%!" body_string;
  return (Yojson.Safe.from_string body_string)
    
let request endpoint request from_json common respond =
  let%lwt response =
    match request with
    | None -> get ~session:common.c_session ~endpoint
    | Some json ->
      let body = Yojson.Safe.to_string json in
      post_with_session ~session:common.c_session ~endpoint ~body
  in
  match API.response_of_yojson from_json response with
  | `Error error ->
    Printf.printf "Failed to convert response from %s: %s\n%!" (Yojson.Safe.to_string response) error;
    respond None
  | `Ok response ->
    respond (Some response) 

let checkSession env common =
  Lwt_unix.run (
    (* ~body:(`Stream (Lwt_stream.of_list ["foo"])) *)
    let%lwt response = get ~session:common.c_session ~endpoint:Endpoints.checkSession in
    match API.response_of_yojson API.checkSession_response_of_yojson response with
    | `Error error ->
      Printf.printf "Failed to extract json: %s\n%!" error;
      return None;
      return ()
    | `Ok { payload = None } ->
      Printf.printf "There was an error in the response: %s\n%!" (Yojson.Safe.to_string response);
      return ()
    | `Ok { payload = Some response } ->
      Printf.printf "id: %s\n%!" response.API._id;
      return (Some response);
      return ()
  )

let checkSession' env common =
  Lwt_unix.run (
    request Endpoints.checkSession None API.checkSession_response_of_yojson common (fun _ -> return ())
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
    | `Ok { payload = None } ->
      Printf.printf "Failed to retrieve token: %s\n%!" (Yojson.Safe.to_string response);
      return ()
    | `Ok { payload = Some token } ->
      Printf.printf "token: %s\n%!" token;
      return (Some token);
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
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
  `S "BUGS"; `P "Check bug reports at http://bugs.example.org.";]

let default_prompt = 
  let doc = "An unofficial standalone command line client for Telkkarista.com" in 
  let man = help_subcommands in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_opts_t)),
  Term.info program_name ~version ~sdocs:"COMMON OPTIONS" ~doc ~man

let cmd_checkSession env =
  let doc = "Check session status" in
  Term.(pure (checkSession env) $ common_opts_t),
  Term.info "checkSession" ~version ~doc

let cmd_login env =
  let doc = "Log into the service" in
  Term.(pure (login env) $ common_opts_t $ username_t $ password_t),
  Term.info "login" ~version ~doc

let main () =
  let subcommands = [
    cmd_checkSession;
    cmd_login;
  ] in
  let env = () in
  match Term.eval_choice default_prompt (List.map (fun x -> x env) subcommands)
  with
  | `Error _ -> exit 1
  | _        -> exit 0

let _ =
  if not !(Sys.interactive) then
    main ()
