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
  status                  : string;
  method_ [@key "method"] : string;
  payload                 : 'a option [@default None];
} [@@deriving of_yojson { strict = false }]

type email = string [@@deriving to_yojson]

type password = string [@@deriving to_yojson]

type login_request = {
  email         : email;
  password      : password;
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

type checkSession_response = {
  lastActivity  : timestamp;
  validUntil    : string;       (* seems to be base-10 yyyymmdd converted to base-16 *)
} [@@deriving of_yojson { strict = false }]

type range_request = {
  from_ [@key "from"] : timestamp;
  to_ [@key "to"]     : timestamp;
} [@@deriving show,to_yojson]

type quality = string [@@deriving show, yojson]

type format = string [@@deriving show, yojson]

type download = {
  quality  : quality;
  bitrate  : int;
  filesize : int;
} [@@deriving show, yojson]

type downloads = download list [@@deriving show, yojson]

type format_downloads = (format * downloads) list [@@deriving show]
let format_downloads_of_yojson = Tools.assoc_of_yojson downloads_of_yojson "API.format_downloads"
let format_downloads_to_yojson = Tools.assoc_to_yojson downloads_to_yojson

type language = string [@@deriving show, yojson]
type title = string [@@deriving show, yojson]

type language_titles = (language * title) list [@@deriving show]

let language_titles_of_yojson = Tools.assoc_of_yojson title_of_yojson "API.title_of_yojson"
let language_titles_to_yojson = Tools.assoc_to_yojson title_to_yojson
  
type pid = string [@@deriving show, yojson]

type record_state = [
  | `Storage
  | `Pending
  | `Recording
  | `Recorded
  | `Broken
  | `Panic
  | `Remuxed
  | `None
] [@@deriving show]

let record_state_of_yojson = function
  | `String "storage" -> `Ok `Storage
  | `String "pending" -> `Ok `Pending
  | `String "recording" -> `Ok `Recording
  | `String "recorded" -> `Ok `Recorded
  | `String "broken" -> `Ok `Broken
  | `String "panic" -> `Ok `Panic
  | `String "remuxed" -> `Ok `Remuxed
  | `String state -> `Error ("API.record_state(" ^ state ^ ")")
  | _ -> `Error ("API.record_state not string")

let record_state_to_yojson = function
  | `Storage -> `String "storage"
  | `Pending -> `String "pending"
  | `Recording -> `String "recording"
  | `Recorded -> `String "recorded"
  | `Broken -> `String "broken"
  | `Panic -> `String "panic"
  | `Remuxed -> `String "remuxed"
  | `None -> `String "none"

type vod = {
  start         : timestamp;
  stop          : timestamp; (* "2015-04-30T21:30:00.000Z" *)
  title         : language_titles;
  subtitle [@key "sub-title"] : language_titles [@default []];
  lastUpdate    : timestamp option [@default None];
  record        : record_state [@default `None];
  storageServer : string option [@default None];
  pid           : pid option [@default None];
  recordpath    : string option [@default None];
  downloads     : format_downloads [@default []];
} [@@deriving show, of_yojson { strict = false }, to_yojson]

type channel = string [@@deriving show, of_yojson]

type programs = vod list [@@deriving show, yojson]

type range_single_response = (channel * programs) [@@deriving show]

type range_response = range_single_response list [@@deriving show]

let range_response_of_yojson = Tools.assoc_of_yojson programs_of_yojson "range response"
let range_response_to_yojson = Tools.assoc_to_yojson programs_to_yojson

type host = string [@@deriving show, of_yojson]

type country = string [@@deriving show, of_yojson]

type status = Up | Down | Other of string [@@deriving show]

let status_of_yojson = function
  | `String "up" -> `Ok Up
  | `String "down" -> `Ok Down
  (* This should probably go away: *)
  | `String other -> `Error ("status_of_json: invalid status: " ^ other)
  | json -> `Error ("status_of_json: not a string: " ^ Yojson.Safe.to_string json)

type cache = {
  host    : host;
  country : country;
  status  : status;
} [@@deriving show, of_yojson { strict = false }]

type cache_response = cache list [@@deriving show, of_yojson]

(* used for queries that don't really have a proper response type yet *)
type json_response = Tools.json [@@deriving show, of_yojson]
    
type client_vod_getUrl_request = {
  pid   : pid;
  path  : string;
  file  : string;
} [@@deriving to_yojson]

type epg_info_request = {
  pid   : pid;
} [@@deriving to_yojson]

type epg_info_response = vod [@@deriving show, yojson { strict = false }]

type speedtest = {
  mbit : float;
  length : float;
} [@@deriving show, of_yojson { strict = false }]

type speedtests = (host * speedtest) list  [@@deriving show]
let speedtests_of_yojson = Tools.assoc_of_yojson speedtest_of_yojson "API.speedtests_of_yojson"

type user_settings_response = {
  speedtests : speedtests;
} [@@deriving show, of_yojson { strict = false }]

type news = {
  title   : string;
  date    : timestamp;
  author  : string;
  content : string;
} [@@deriving show, of_yojson { strict = false }]

type news_get_response = news list [@@deriving show, of_yojson]

type epg_titles_response = language_titles list [@@deriving show, of_yojson]

type payment_getPackages_response = Tools.json [@@deriving show, of_yojson]
