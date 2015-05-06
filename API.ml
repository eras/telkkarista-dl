open Batteries

type error = [
  | `Email_in_use [@name "email_in_use"]
  | `Email_not_verified [@name "email_not_verified"]
  | `Error_in_logout [@name "error_in_logout"]
  | `Error_in_query [@name "error_in_query"]
  | `Failed [@name "failed"]
  | `Failed_insert_user [@name "failed_insert_user"]
  | `Failed_to_start_session [@name "failed_to_start_session"]
  | `Hash_error [@name "hash_error"]
  | `Invalid_email [@name "invalid_email"]
  | `Invalid_password [@name "invalid_password"]
  | `No_session_found [@name "no_session_found"]
  | `No_sessions [@name "no_sessions"]
  | `No_user_found [@name "no_user_found"]
  | `Not_found [@name "not_found"]
  (* | `Null [@name "null"] *)
  | `Password_too_short [@name "password_too_short"]
  | `Pid_not_found [@name "pid_not_found"]
  | `Pipe [@name "pipe"]
  | `Salt_error [@name "salt_error"]
  | `Search_error [@name "search_error"]
  | `Unknown_error [@name "unknown_error"]
] [@@deriving of_yojson]

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
} [@@deriving of_yojson { strict = false }]

type login_request = {
  email         : string;
  password      : string;
} [@@deriving to_yojson]

type session_token = string [@@deriving of_yojson]

(* "status"     : "ok" *)
(* "code"       : "login_ok" *)
type login_response = session_token [@@deriving of_yojson]

type timestamp = string [@@deriving yojson]

type range_request = {
  begin_ [@key "begin"] : timestamp;
  end_ [@key "end"]     : timestamp;
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
  start         : timestamp;
  stop          : timestamp; (* "2015-04-30T21:30:00.000Z" *)
  title         : title;
  record        : string;
  storageServer : string;
  pid           : pid;
} [@@deriving of_yojson { strict = false }]

type channel = string [@@deriving of_yojson]

type programs = vod list [@@deriving of_yojson]

type range_single_response = (channel * programs)

type range_response = range_single_response list

let assoc_of_yojson f error (json : Yojson.Safe.json) =
  match json with
  | `Assoc assoc ->
    ( let results = List.map (fun (k, v) -> (k, f v)) assoc in
      let ok, errors =
        Tools.partition_map (
          function
          | (key, `Ok value) -> `Left (key, value)
          | (key, `Error error) -> `Right error)
          results
      in
      match errors with
      | [] -> `Ok ok
      | errors -> `Error (String.concat " & " errors) )
  | _ -> `Error error

let range_response_of_yojson = assoc_of_yojson programs_of_yojson "Invalid JSON for range response"
