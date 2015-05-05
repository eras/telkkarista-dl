open Lwt.Infix
open Cmdliner

let program_name = "telkkarista"
let version = "0.1"
let base = "http://api.telkkarista.com"

let return = Lwt.return

let (/^) a b = a ^ "/" ^ b

type environment = [`Environment] (* temporary *)

type common = {
  c_session : API.session_token; (* session id is kept here *)
  c_env     : environment;       (* auxiliary environment data (ie. loaded configuration, location of configuration) *)
}

(* Converting JSON to proper data types uses this type *)
type 'a of_json_result = [ `Error of string | `Ok of 'a ]

(* This makes 'a Lwt.t non-abstract for the purposes of GADT *)
type 'a lwt_result = LwtResult of 'a Lwt.t

type ('session, 'a, 'b) result = ('session -> 'a -> 'b option lwt_result)

type _ request =
  (* GetRequest has no parameters, but does have a response, ie. checkSession *)
  | GetRequest: (Yojson.Safe.json -> 'b of_json_result) -> (API.session_token, unit, 'b) result request

  (* PostRequestNoSession has parameters and response, but does not use session identifier, ie. login *)
  | PostRequestNoSession: (('a -> Yojson.Safe.json) * (Yojson.Safe.json -> 'b of_json_result)) -> (unit, 'a, 'b) result request

  (* PostRequest is the typically used request with both input and output and session identifier *)
  | PostRequest: (('a -> Yojson.Safe.json) * (Yojson.Safe.json -> 'b of_json_result)) -> (API.session_token, 'a, 'b) result request

(* Each request has this header (except PostRequestNoSession) *)
let session_header session = ["X-SESSION", session]

(* TODO: remove *)
let post ~headers ~endpoint ~body =
  let headers = Cohttp.Header.of_list (("Content-Length", Printf.sprintf "%d" (String.length body))::headers) in
  let%lwt (_, body) = Cohttp_lwt_unix.Client.post ~body:(`Stream (Lwt_stream.of_list [body])) ~headers endpoint in
  let%lwt body_string = Cohttp_lwt_body.to_string body in
  return (Yojson.Safe.from_string body_string)

(* TODO: remove *)
let post_with_session ~session ~endpoint ~body = post ~headers:(session_header session) ~endpoint ~body

(* TODO: remove *)
let post_without_session ~endpoint ~body = post ~headers:[] ~endpoint ~body

(* TODO: remove *)
let get ~session ~endpoint =
  let headers = Cohttp.Header.of_list (session_header session) in
  let%lwt (_, body) = Cohttp_lwt_unix.Client.get ~headers endpoint in
  let%lwt body_string = Cohttp_lwt_body.to_string body in
  Printf.printf "Response: %s\n%!" body_string;
  return (Yojson.Safe.from_string body_string)
    
(* TODO: remove *)
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

(* API end points with type safe interfaces *)
module Endpoints =
struct
  let version = 1
  
  let endpoint_uri service = Uri.of_string (base /^ Printf.sprintf "%d" version /^ service)

  (* Construct a request object for the given resource. The return
     type [req] results in two more arguments: the session id (or ()
     if no session as with PostRequestNoSession) and data argument (or
     () if no data argument as with GetRequest). The return type will
     be packaged to LwtResult due to GADT reasons; use [lwt] to
     unpackage it. *)
  let request (type req) (type session) uri (request : req request) : req =
    let handle_response json_to_response response =
      match API.response_of_yojson json_to_response response with
      (* We failed to decode then JSON; TODO: better error handling *)
      | `Error error ->
        Printf.printf "Failed to convert response from %s: %s\n%!" (Yojson.Safe.to_string response) error;
        return None
      (* We succeeded in decoding the JSON, but it indicated an error. TODO: better error handling *)
      | `Ok { API.code = code; payload = None } ->
        Printf.printf "Error %s in %s\n%!" code (Yojson.Safe.to_string response);
        return None
      (* Everything's awesome! *)
      | `Ok { API.payload = Some payload } ->
        return (Some payload)
    in
    match request with
    | GetRequest json_to_response -> fun (session) () ->
      LwtResult (get ~session ~endpoint:uri >>= handle_response json_to_response)
    | PostRequestNoSession (request_to_json, json_to_response) -> fun () arg ->
      let body = Yojson.Safe.to_string (request_to_json arg) in
      LwtResult (post_without_session ~endpoint:uri ~body >>= handle_response json_to_response)
    | PostRequest (request_to_json, json_to_response) -> fun session arg ->
      let body = Yojson.Safe.to_string (request_to_json arg) in
      LwtResult (post_with_session ~session ~endpoint:uri ~body >>= handle_response json_to_response)

  (* let's consider this a special case for how *)
  let login_uri = endpoint_uri "user/login"

  (* Unpacks the LwtResult-packaged Lwt.t from [request] *)
  let lwt f session argument =
    let LwtResult lwt = f session argument in
    lwt    

  let login_request =
    lwt @@ request
      (endpoint_uri "user/login")
      (PostRequestNoSession (API.login_request_to_yojson,
                             API.login_response_of_yojson))

  let checkSession = endpoint_uri "user/checkSession"
  
  let checkSession_request =
    lwt @@ request
      (endpoint_uri "user/checkSession")
      (GetRequest (API.checkSession_response_of_yojson))

  let range = endpoint_uri "epg/range"

  let range_request =
    lwt @@ request
      (endpoint_uri "epg/range")
      (PostRequest (API.range_request_to_yojson,
                    API.range_response_of_yojson))
end

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

let range common begin_ end_ =
  Lwt_unix.run (
    request Endpoints.range (Some (API.range_request_to_yojson { API.begin_; end_ })) API.range_response_of_yojson common (fun _ -> return ())
  )

let login common email password =
  Lwt_unix.run (
    let body = Yojson.Safe.to_string @@ API.login_request_to_yojson { API.email; password } in
    let%lwt response = post_without_session ~endpoint:Endpoints.login_uri ~body in
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
