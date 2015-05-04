open Lwt.Infix
open Cmdliner

let program_name = "telkkarista"
let version = "0.1"
let base = "http://api.telkkarista.com"

let return = Lwt.return

let (/^) a b = a ^ "/" ^ b

type 'env common = {
  c_session : string;
  c_env     : [`Environment];
}

module API =
struct
  type 'a response = {
    status        : string;
    code          : string;
    payload       : 'a option [@default None];
  } [@@deriving of_yojson { strict = false }]

  type checkSession_response = {
    _id           : string;
    user_id       : string;
    lastActivity  : string;
    meta          : Yojson.Safe.json;
    email         : string;
  } [@@deriving of_yojson]

  type login_request = {
    email         : string;
    password      : string;
  } [@@deriving to_yojson]

  (* "status"     : "ok" *)
  (* "code"       : "login_ok" *)
  type login_response = string [@@deriving of_yojson]

  type range_request = {
    t0 [@key "begin"] : string;
    t1 [@key "end"]   : string;
  } [@@deriving to_yojson]

  type quality = string [@@deriving of_yojson]
  
  type download_formats = {
    mp4           : quality list [@default []];
    ts            : quality list [@default []]
  } [@@deriving of_yojson { strict = false }]

  type title = {
    fi            : string [@default ""];
    sv            : string [@default ""];
  } [@@deriving of_yojson { strict = false }]

  type pid = string [@@deriving of_yojson]
  
  type vod = {
    start         : string;
    stop          : string; (* "2015-04-30T21:30:00.000Z" *)
    title         : title;
    record        : string;
    storageServer : string;
    pid           : pid;
  } [@@deriving of_yojson]

  type channel = string [@@deriving of_yojson]
  
  type programs = vod list [@@deriving of_yojson]

  type range_single_response = (channel * programs)

  type range_response = range_single_response list

  let assoc_of_yojson f error (json : Yojson.Safe.json) =
    match json with
    | `Assoc assoc -> `Ok (List.map (fun (k, v) -> (k, f v)) assoc)
    | _ -> `Error error
  
  let range_response_of_yojson = assoc_of_yojson programs_of_yojson "Invalid JSON for range response"
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

let checkSession common =
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

let checkSession' common =
  Lwt_unix.run (
    request Endpoints.checkSession None API.checkSession_response_of_yojson common (fun _ -> return ())
  )

let login common email password =
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

let common_opts env session = { c_session = session;
                                c_env = env }

let common_opts_t env =
  let docs = "COMMON OPTIONS" in 
  let session = 
    let doc = "Set session id" in
    Arg.(value & opt string "" & info ["s"; "session"] ~docs ~doc)
  in
  Term.(pure (common_opts env) $ session)

let username_t =
  let doc = "User name (email). This is required." in
  Arg.(required & opt (some string) None & info ["u"; "user"] ~doc)

let password_t =
  let doc = "Password. This is required." in
  Arg.(required & opt (some string) None & info ["p"; "pass"] ~doc)

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

let cmd_checkSession env =
  let doc = "Check session status" in
  Term.(pure checkSession $ common_opts_t env),
  Term.info "checkSession" ~version ~doc

let cmd_login env =
  let doc = "Log into the service" in
  Term.(pure login $ common_opts_t env $ username_t $ password_t),
  Term.info "login" ~version ~doc

let main () =
  let subcommands = [
    cmd_checkSession;
    cmd_login;
  ] in
  let env = `Environment in
  match Term.eval_choice (default_prompt env) (List.map (fun x -> x env) subcommands)
  with
  | `Error _ -> exit 1
  | _        -> exit 0

let _ =
  if not !(Sys.interactive) then
    main ()
