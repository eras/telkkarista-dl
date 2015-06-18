open Batteries
open Lwt.Infix

let base = "https://api.telkkarista.com"

let return = Lwt.return

let (^/) a b = a ^ "/" ^ b

(* Converting JSON to proper data types uses this type *)
type 'a of_json_result = [ `Error of string | `Ok of 'a ]

type 'session update_session = unit -> 'session Lwt.t
type 'a value = Ok of 'a | Invalid_response | Error of string
type ('session, 'request, 'response) result = 'session -> 'session update_session -> 'request -> 'response value Lwt.t

type ('session, 'request, 'response) request =
  (* GetRequest has no parameters, but does have a response, ie. checkSession *)
  | GetRequest: (Yojson.Safe.json -> 'b of_json_result) -> (API.session_token, unit, 'b) request

  (* PostRequestNoSession has parameters and response, but does not use session identifier, ie. login *)
  | PostRequestNoSession: (('a -> Yojson.Safe.json) * (Yojson.Safe.json -> 'b of_json_result)) -> (unit, 'a, 'b) request

  (* PostRequest is the typically used request with both input and output and session identifier *)
  | PostRequest: (('a -> Yojson.Safe.json) * (Yojson.Safe.json -> 'b of_json_result)) -> (API.session_token, 'a, 'b) request

(* API end points with type safe interfaces *)
let version = 1

let endpoint_uri service = Uri.of_string (base ^/ Printf.sprintf "%d" version ^/ service)

(* Each request has this header (except PostRequestNoSession) *)
let session_header session = ["X-SESSION", session]

let debug = try Sys.getenv "TELKKARISTA_DEBUG" = "1" with Not_found -> false

(* Issues a general POST request to the endpoint *)
let post ~headers ~endpoint ~body =
  if debug then Printf.eprintf "Requesting: %s\n%!" body;
  let headers = Cohttp.Header.of_list (("Content-Length", Printf.sprintf "%d" (String.length body))::headers) in
  let%lwt (_, body) = Cohttp_lwt_unix.Client.post ~body:(`Stream (Lwt_stream.of_list [body])) ~headers endpoint in
  let%lwt body_string = Cohttp_lwt_body.to_string body in
  if debug then Printf.eprintf "Response: %s\n%!" body_string;
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
  if debug then Printf.eprintf "Response: %s\n%!" body_string;
  return (Yojson.Safe.from_string body_string)

let retry session0 update_session request =
  let rec loop retries_left session =
    let%lwt response = request ~session in
    match response with
    | `Assoc kvs -> (
        match List.assoc "status" kvs with
        | `String "error" when retries_left > 0 ->
          let%lwt new_session = update_session () in
          loop (retries_left - 1) new_session
        | `String "error" ->
          Printf.eprintf "Failed to relogin\n%!";
          assert false
        | other -> return response
        | exception Not_found ->
          Printf.eprintf "Response is missing status and code\n%!";
          assert false
      )
    | _ ->
      Printf.eprintf "Response is missing associative container\n%!";
      assert false
  in
  loop 1 session0

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
      Printf.eprintf "Failed to convert response due to %s from %s\n%!" error (Yojson.Safe.to_string response);
      return (Error error)
    (* We succeeded in decoding the JSON, but it indicated an error. TODO: better error handling *)
    | `Ok { API.status; payload = None } ->
      Printf.eprintf "Error %s in %s\n%!" status (Yojson.Safe.to_string response);
      return Invalid_response
    (* Everything's awesome! *)
    | `Ok { API.payload = Some payload } ->
      return (Ok payload)
  in
  match request with
  | GetRequest json_to_response -> fun session update_session _ ->
    retry session update_session (get ~endpoint:uri) >>= handle_response json_to_response
  | PostRequestNoSession (request_to_json, json_to_response) -> fun _session _update_session arg ->
    let body = Yojson.Safe.to_string (request_to_json arg) in
    post_without_session ~endpoint:uri ~body >>= handle_response json_to_response
  | PostRequest (request_to_json, json_to_response) -> fun session update_session arg ->
    let body = Yojson.Safe.to_string (request_to_json arg) in
    retry session update_session (post_with_session ~endpoint:uri ~body) >>= handle_response json_to_response

(* let's consider this a special case for how *)
let user_login =
  request
    (endpoint_uri "user/login")
    (PostRequestNoSession (API.login_request_to_yojson,
                           API.login_response_of_yojson))

let user_checkSession =
  request
    (endpoint_uri "user/checkSession")
    (GetRequest (API.checkSession_response_of_yojson))

let epg_range =
  request
    (endpoint_uri "epg/range")
    (PostRequest (API.range_request_to_yojson,
                  API.range_response_of_yojson))

let download_url cache_server (session_token : API.session_token) format quality vod basename =
  match vod.API.recordpath, List.mem_assoc format vod.API.downloads with
  | None, _ -> None
  | _, false -> None
  | Some recordpath, true ->
    let downloads = List.assoc format vod.API.downloads in
    if List.exists (fun download -> download.API.quality = quality) downloads then
      Some ("http://" ^ cache_server ^/ session_token ^/ "vod" ^ recordpath ^ quality ^/ basename ^ "." ^ format)
    else
      None

let cache_get =
  request
    (endpoint_uri "cache/get")
    (GetRequest API.cache_response_of_yojson)

let client_vod_getUrl =
  request
    (endpoint_uri "client/vod/getUrl")
    (PostRequest (API.client_vod_getUrl_request_to_yojson,
                  API.json_response_of_yojson))

let epg_info =
  request
    (endpoint_uri "epg/info")
    (PostRequest (API.epg_info_request_to_yojson,
                  API.epg_info_response_of_yojson))

let user_settings =
  request
    (endpoint_uri "user/settings")
    (GetRequest (API.user_settings_response_of_yojson))
