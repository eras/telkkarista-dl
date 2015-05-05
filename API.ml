open Batteries

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
} [@@deriving of_yojson]

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
