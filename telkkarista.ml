open Batteries
open Lwt.Infix
open Cmdliner

let program_name = "telkkarista"
let version = "0.1"
let base = "http://api.telkkarista.com"

let return = Lwt.return

let (^/) a b = a ^ "/" ^ b

type environment = [`Environment] (* temporary *)

type common = {
  c_session : API.session_token; (* session id is kept here *)
  c_env     : environment;       (* auxiliary environment data (ie. loaded configuration, location of configuration) *)
}

(* Converting JSON to proper data types uses this type *)
type 'a of_json_result = [ `Error of string | `Ok of 'a ]

type ('session, 'request, 'response) result = ('session -> 'request -> 'response option Lwt.t)

type ('session, 'request, 'response) request =
  (* GetRequest has no parameters, but does have a response, ie. checkSession *)
  | GetRequest: (Yojson.Safe.json -> 'b of_json_result) -> (API.session_token, unit, 'b) request

  (* PostRequestNoSession has parameters and response, but does not use session identifier, ie. login *)
  | PostRequestNoSession: (('a -> Yojson.Safe.json) * (Yojson.Safe.json -> 'b of_json_result)) -> (unit, 'a, 'b) request

  (* PostRequest is the typically used request with both input and output and session identifier *)
  | PostRequest: (('a -> Yojson.Safe.json) * (Yojson.Safe.json -> 'b of_json_result)) -> (API.session_token, 'a, 'b) request

(* API end points with type safe interfaces *)
module Endpoints =
struct
  let version = 1
  
  let endpoint_uri service = Uri.of_string (base ^/ Printf.sprintf "%d" version ^/ service)

  (* Each request has this header (except PostRequestNoSession) *)
  let session_header session = ["X-SESSION", session]

  (* Issues a general POST request to the endpoint *)
  let post ~headers ~endpoint ~body =
    Printf.printf "Requesting: %s\n%!" body;
    let headers = Cohttp.Header.of_list (("Content-Length", Printf.sprintf "%d" (String.length body))::headers) in
    let%lwt (_, body) = Cohttp_lwt_unix.Client.post ~body:(`Stream (Lwt_stream.of_list [body])) ~headers endpoint in
    let%lwt body_string = Cohttp_lwt_body.to_string body in
    Printf.printf "Response: %s\n%!" body_string;
    return (Yojson.Safe.from_string body_string)

  (* Issues a POST request with session id to the endpoint *)
  let post_with_session ~session ~endpoint ~body = post ~headers:(session_header session) ~endpoint ~body

  (* Issues a POST request without session id to the endpoint *)
  let post_without_session ~endpoint ~body = post ~headers:[] ~endpoint ~body

  (* Issues a GET request with session id to the endpoint *)
  let get ~session ~endpoint =
    let headers = Cohttp.Header.of_list (session_header session) in
    let%lwt (_, body) = Cohttp_lwt_unix.Client.get ~headers endpoint in
    let%lwt body_string = Cohttp_lwt_body.to_string body in
    Printf.printf "Response: %s\n%!" body_string;
    return (Yojson.Safe.from_string body_string)

  (* Construct a request object for the given resource. The return
     type [req] results in two more arguments: the session id (or ()
     if no session as with PostRequestNoSession) and data argument (or
     () if no data argument as with GetRequest). The return type will
     be packaged to LwtResult due to GADT reasons; use [lwt] to
     unpackage it. *)
  let request (type session) (type request_) (type response) uri (request : (session, request_, response) request) : (session, request_, response) result =
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
      get ~session ~endpoint:uri >>= handle_response json_to_response
    | PostRequestNoSession (request_to_json, json_to_response) -> fun () arg ->
      let body = Yojson.Safe.to_string (request_to_json arg) in
      post_without_session ~endpoint:uri ~body >>= handle_response json_to_response
    | PostRequest (request_to_json, json_to_response) -> fun session arg ->
      let body = Yojson.Safe.to_string (request_to_json arg) in
      post_with_session ~session ~endpoint:uri ~body >>= handle_response json_to_response

  (* let's consider this a special case for how *)
  let login_uri = endpoint_uri "user/login"

  let login_request =
    request
      (endpoint_uri "user/login")
      (PostRequestNoSession (API.login_request_to_yojson,
                             API.login_response_of_yojson))

  let checkSession = endpoint_uri "user/checkSession"
  
  let checkSession_request =
    request
      (endpoint_uri "user/checkSession")
      (GetRequest (API.checkSession_response_of_yojson))

  let range = endpoint_uri "epg/range"

  let range_request =
    request
      (endpoint_uri "epg/range")
      (PostRequest (API.range_request_to_yojson,
                    API.range_response_of_yojson))

  let download_url server (session_token : API.session_token) format quality vod =
    match vod.API.recordpath, List.mem_assoc format vod.API.downloadFormats with
    | None, _ -> None
    | _, false -> None
    | Some recordpath, true ->
      let qualities = List.assoc format vod.API.downloadFormats in
      if List.mem quality qualities then
        Some ("http://" ^/ server ^/ session_token ^/ "vod" ^ recordpath ^ format ^ "." ^ quality)
      else
        None
end

let interactive_request endpoint session arg show =
  let%lwt response = endpoint session arg in
  match response with
  | None ->
    Printf.printf "There was an error\n%!";
    return ()
  | Some response ->
    Printf.printf "%s\n%!" (show response);
    return ()

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
    interactive_request Endpoints.checkSession_request common.c_session () @@
    fun response -> response.API._id
  in
  let doc = "Check session status" in
  Term.(pure (lwt1 checkSession) $ common_opts_t env),
  Term.info "checkSession" ~version ~doc

let cmd_login env =
  let login common email password =
    interactive_request Endpoints.login_request () { API.email; password } @@
    fun token -> token
  in
  let doc = "Log into the service" in
  Term.(pure (lwt3 login) $ common_opts_t env $ username_t $ password_t),
  Term.info "login" ~version ~doc

let cmd_list env =
  let range common from_ to_ =
    interactive_request Endpoints.range_request common.c_session { API.from_; to_ } @@
    fun response -> API.show_range_response response
  in
  let doc = "List programs from given time range" in
  Term.(pure (lwt3 range) $ common_opts_t env $ begin_t $ end_t),
  Term.info "list" ~version ~doc

let main () =
  let subcommands = [
    cmd_checkSession;
    cmd_login;
    cmd_list;
  ] in
  let env = `Environment in
  match Term.eval_choice (default_prompt env) (List.map (fun x -> x env) subcommands)
  with
  | `Error _ -> exit 1
  | _        -> exit 0

let _ =
  if not !(Sys.interactive) then
    main ()
