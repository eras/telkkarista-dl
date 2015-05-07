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

type timestamp = float

let timestamp_to_yojson timestamp = `String (ISO8601.Permissive.string_of_datetimezone (timestamp, 0.0))
let timestamp_of_yojson yojson =
  match yojson with
  | `String datetime_str ->
    ( try `Ok (ISO8601.Permissive.datetime datetime_str)
      with _ -> `Error "API.timestamp_of_yojson" )
  | _ -> `Error "API.timestamp_of_yojson"

let show_timestamp timestamp =
  Printf.sprintf "%s" (ISO8601.Permissive.string_of_datetimezone (timestamp, -. float (Netdate.get_localzone ()) *. 60.0))

let pp_timestamp fmt timestamp = Format.fprintf fmt "%s" (show_timestamp timestamp)

type range_request = {
  from_ [@key "from"] : timestamp;
  to_ [@key "to"]     : timestamp;
} [@@deriving show,to_yojson]

type quality = string [@@deriving of_yojson]

type download_formats = {
  mp4           : quality list [@default []];
  ts            : quality list [@default []]
} [@@deriving of_yojson { strict = false }]

type language = string [@@deriving show, of_yojson]
type title = string [@@deriving show, of_yojson]

type language_titles = (language * title) list [@@deriving show]

let language_titles_of_yojson = Tools.assoc_of_yojson title_of_yojson "API.title_of_yojson"
  
type pid = string [@@deriving show, of_yojson]

type record_state = [
  | `Storage
  | `Pending
  | `Recording
  | `None
] [@@deriving show]

let record_state_of_yojson = function
  | `String "storage" -> `Ok `Storage
  | `String "pending" -> `Ok `Pending
  | `String "recording" -> `Ok `Recording
  | _ -> `Error "API.record_state"

type vod = {
  start         : timestamp;
  stop          : timestamp; (* "2015-04-30T21:30:00.000Z" *)
  title         : language_titles;
  record        : record_state [@default `None];
  storageServer : string option [@default None];
  pid           : pid;
} [@@deriving show, of_yojson { strict = false }]

type channel = string [@@deriving show, of_yojson]

type programs = vod list [@@deriving show, of_yojson]

type range_single_response = (channel * programs)

type range_response = range_single_response list

let range_response_of_yojson = Tools.assoc_of_yojson programs_of_yojson "range response"

let show_range_response (range_response : range_response) =
  String.concat ", " @@
  List.map (fun (channel, programs) -> show_channel channel ^ ": " ^ show_programs programs) range_response
   

