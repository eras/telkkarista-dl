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
  begin_ [@key "begin"] : string;
  end_ [@key "end"]     : string;
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
