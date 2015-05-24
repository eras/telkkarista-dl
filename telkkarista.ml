module Text' = Text
open Batteries
module Text = Text'
open Lwt.Infix
open Cmdliner

let program_name = "telkkarista-dl"
let version = Version.version

let return = Lwt.return

let renegotiate_session : Common.common -> _ Endpoints.update_session = fun common ->
  let env = common.Common.c_env in
  match Persist.get_email_password common.Common.c_env.Common.e_persist with
  | None -> (fun () -> 
      Printf.printf "Cannot acquire token; no saved credentials\n";
      assert false
    )
  | Some (email, password) -> (fun () ->
      match%lwt Endpoints.user_login () (const (return ())) { API.email; password } with
      | Endpoints.Error _ | Endpoints.Invalid_response ->
        Printf.printf "Failed to acquire token\n%!";
        assert false
      | Endpoints.Ok token ->
        Persist.set_session env.Common.e_persist token;
        return token
    )

let interactive_single_shot endpoint session arg show =
  let%lwt response = endpoint session (const (return ())) arg in
  match response with
  | None ->
    Printf.printf "There was an error\n%!";
    return ()
  | Some response ->
    Printf.printf "%s\n%!" (show response);
    return ()

let interactive_request (common : Common.common) (endpoint : (_, _, _) Endpoints.result) session arg show =
  let%lwt response = endpoint session (renegotiate_session common) arg in
  match response with
  | Endpoints.Error error ->
    Printf.printf "Error: %s\n%!" error;
    return ()
  | Endpoints.Invalid_response ->
    Printf.printf "Invalid response\n%!";
    return ()
  | Endpoints.Ok response ->
    Printf.printf "%s\n%!" (show response);
    return ()

let common_opts env session = {
  Common.c_session = session;
  c_env = env;
}

let common_opts_t env =
  let docs = "COMMON OPTIONS" in 
  let session = 
    let doc = "Set session id" in
    Arg.(value & opt string (Option.default "" (Persist.get_session env.Common.e_persist)) & info ["s"; "session"] ~docv:"TOKEN" ~docs ~doc)
  in
  Term.(pure (common_opts env) $ session)

let req_optional kind default info_ =
  match default with
  | None -> Arg.(required & opt (some kind) None & info_)
  | Some default -> Arg.(value & opt kind default & info_)

let username_t persist =
  let doc = "User name (email)." in
  req_optional Arg.string (Option.map fst (Persist.get_email_password persist)) (Arg.info ["u"; "user"] ~docv:"EMAIL" ~doc)

let password_t persist =
  let doc = "Password." in
  req_optional Arg.string (Option.map snd (Persist.get_email_password persist)) (Arg.info ["p"; "pass"] ~docv:"PASSWORD" ~doc)

let pid_t =
  let doc = "Program ID. This is required." in
  req_optional Arg.string None (Arg.info ["p"; "pid"] ~docv:"PID" ~doc)

let pids_t =
  let doc = "Program IDs." in
  Arg.(value & pos_all string [] & Arg.info [] ~docv:"PID" ~doc)

let format_t =
  let doc = "Format for download." in
  Arg.(value & opt (some string) None & (Arg.info ["f"; "format"] ~docv:"FORMAT" ~doc))

let quality_t =
  let doc = "Quality for download." in
  Arg.(value & opt (some string) None & (Arg.info ["q"; "quality"] ~docv:"QUALITY" ~doc))

let best_cache persist =
  match
    Persist.get_cache_servers persist |>
    List.sort (fun (_, a) (_, b) -> compare b.API.mbit a.API.mbit)
  with
  | (fastest, _)::_ -> Some fastest
  | _ -> None

let cache_server_t persist =
  let doc = "Cache server for download." in
  req_optional Arg.string (best_cache persist) (Arg.info ["cache"] ~docv:"SERVER" ~doc)

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
  Arg.(value & opt (some datetime) None & info ["b"; "begin"] ~docv:"ISO8601 TIME" ~doc)

let end_t =
  let doc = "End time stamp of the range." in
  Arg.(value & opt (some datetime) None & info ["e"; "end"] ~docv:"ISO8601 TIME" ~doc)

let save_file_t =
  let doc = "Save results as to the given file." in
  Arg.(value & opt (some string) None & info ["s"; "save"] ~docv:"FILE" ~doc)

let load_file_t =
  let doc = "Load data to process from the give file" in
  Arg.(value & opt (some string) None & info ["l"; "load"] ~docv:"FILE" ~doc)

let channels_t =
  let doc = "Limit to this channel" in
  Arg.(value & opt_all string [] & info ["c"; "channel"] ~docv:"CHANNEL" ~doc)

let name_filter_t =
  let doc = "Name filtering. List only shows that have this string in their name." in
  Arg.(value & opt (some string) None & info ["n"; "name"] ~docv:"NAME" ~doc)

let description_filter_t =
  let doc = "Description filtering. List only shows that have this string in their description. \
             This is useless, though, as the TV listing doesn't include this information.." in
  Arg.(value & opt (some string) None & info ["d"; "description"] ~docv:"DESCRIPTION" ~doc)

let help_subcommands = [
  `S "COMMON OPTIONS";
  `P "These options are common to all commands.";
  `S "MORE HELP";
  `P "Use $(b,$(mname) COMMAND --help) for help on a single command.";
  `S "EXAMPLES";
  `P "Log in to the server the first time:";
  `P "$(b,% $(mname) login -u user@name -p password)";
  `P "This will store used username and password for re-logging in later; plain $(b,$(mname) login) without -u and -p is then sufficient.";
  `P "Retrieving list of TV shows:";
  `P "$(b,% $(mname) list -b '2015-01-01T10:00:00' -e '2015-01-01T20:00:00')";
  `P "The program identifier can be used with $(b,info) and $(b,url). To save the list to a file in machine-readable format you may use the switch $(b,--save):";
  `P "$(b,% $(mname) list -b '2015-01-01T10:00:00' -e '2015-01-01T20:00:00' --save results.json)";
  `P "which may later on be read with --load (in which case the time range cannot be used).";
  `P "With $(b,$(mname) list) one may use further filters such as $(b,-c yletv1) for limiting search to a single channel or $(b,-n dredd) for searching case-insensitively for a certain string within the names of the programs. Note that regardless which search options have been used with $(b,list --save), the saved results will be unfiltered.";
  `P "$(b,% $(mname) list -b '2015-01-01T10:00:00' -e '2015-01-01T20:00:00' --save results.json -n dredd)";
  `P "$(b,% $(mname) list --load results.json -n judge)";
  `P "To find out the download URL of a program, use $(b,$(mname) url):";
  `P "$(b,% $(mname) url --pid 554e2bbbe3398c93fe916428)";
  `P "To download a program, or programs directly:";
  `P "$(b,% $(mname) download 554e2bbbe3398c93fe916428 557e2bbbe3398c93fe916795)";
  `P "Files will be named after the programs.";
  `S "FILES";
  `P "$(b,~/.config/telkkarista/session) or $(qb,~/.telkkarista/session) contain the current effective configuration.";
  `S "ENVIRONMENT";
  `P "$(b,TELKKARISTA_DEBUG) - when set to 1, received responses from the server will be output to standard output.";
  (* `S "BUGS"; `P "Check bug reports at http://bugs.example.org."; *)
]

let default_prompt env = 
  let doc = "An unofficial standalone command line client for Telkkarista.com" in 
  let man = help_subcommands in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_opts_t env)),
  Term.info program_name ~version ~sdocs:"COMMON OPTIONS" ~doc ~man

let cmd_checkSession env =
  let checkSession common =
    interactive_request common Endpoints.user_checkSession common.Common.c_session () @@
    fun response -> response.API._id
  in
  let doc = "Check session status" in
  Term.(pure checkSession $ common_opts_t env),
  Term.info "checkSession" ~doc

let update_persisted_settings env settings =
  List.iter
    (function
      | API.Speedtests speedtests -> Persist.set_cache_servers env.Common.e_persist speedtests
      | Other _ -> ())
    settings

let cmd_login env =
  let login common email password =
    match%lwt Endpoints.user_login () (const (return ())) { API.email; password } with
    | Endpoints.Invalid_response ->
      Printf.printf "Failed to log in: invalid response\n";
      return ()
    | Endpoints.Error error ->
      Printf.printf "Failed to log in: %s\n" error;
      return ()
    | Endpoints.Ok token ->
      Printf.printf "Logged in\n%!";
      Persist.set_email_password env.Common.e_persist email password;
      Persist.set_session env.Common.e_persist token;
      let%lwt () = match%lwt Endpoints.user_settings token (renegotiate_session common) () with
        | Endpoints.Invalid_response
        | Endpoints.Error _ ->
          Printf.printf "Failed to retrieve settings\n%!";
          return ()
        | Endpoints.Ok settings ->
          update_persisted_settings env settings;
          return ()
      in
      return ()
  in
  let doc = "Log into the service" in
  Term.(pure login $ common_opts_t env $ username_t env.Common.e_persist $ password_t env.Common.e_persist),
  Term.info "login" ~doc

let cmd_settings env =
  let settings common =
    interactive_request common Endpoints.user_settings common.Common.c_session () @@
    fun settings ->
    update_persisted_settings env settings;
    API.show_user_settings_response settings
  in
  let doc = "Retrieve settings" in
  Term.(pure settings $ common_opts_t env),
  Term.info "settings" ~doc

module TimeMap = Map.Make (struct type t = float let compare = compare end)
module ChannelMap = Map.Make (struct type t = API.channel let compare = compare end)

let qualities = ["1080p"; "720p"; "highest"; "hi"]

let format_qualities_of_formats formats =
  formats
  |> List.map (fun (format, quality) -> List.map (fun q -> (format, q)) quality)
  |> List.concat

let format_quality_preference = format_qualities_of_formats [("ts", qualities); ("mp4", qualities)]

let language_preference = ["fi"; "sv"]

let compare_by f a b = compare (f a) (f b)

(* Very slow to use.. *)
let index_of xs x =
  try fst @@ List.findi (fun _ y -> x = y) xs
  with Not_found -> List.length xs

let title_for vod =
  let titles = vod.API.title in
  match List.sort (compare_by (index_of language_preference % fst)) titles with
  | (_, preferred)::_ -> Some preferred
  | [] -> None

let subtitle_for vod =
  let subtitles = vod.API.subtitle in
  match List.sort (compare_by (index_of language_preference % fst)) subtitles with
  | (_, preferred)::_ -> Some preferred
  | [] -> None

let format_quality_for preference vod =
  let formats = format_qualities_of_formats vod.API.downloadFormats in
  match List.sort (compare_by (index_of preference)) formats with
  | preferred::_ -> Some preferred
  | [] -> None

let split_string_to_chunks len str =
  let rec loop index strs =
    if index < Text.length str then
      let end_point = min (index + len) (Text.length str) in
      let len' = (end_point - index) in
      (* Printf.printf "%s %d %d\n" str index len'; *)
      loop
        end_point
        (if len' > 0
         then Text.sub str index len'::strs
         else strs)
    else
      List.rev strs
  in
  loop 0 []

let output_program_table (input : (string * API.vod list) list) =
  let open Containers_misc.PrintBox in
  set_string_len Text.length;
  let channels = List.map fst input in
  let vods = List.concat @@ List.map snd input in
  let begins = List.map (fun vod -> vod.API.start) vods |> List.sort compare |> List.unique in
  let programs_by_channel_and_begin =
    List.fold_left (
      fun channel_map (channel, vods) ->
        let time_map = List.fold_left (fun time_map vod -> TimeMap.add vod.API.start vod time_map) TimeMap.empty vods in
        ChannelMap.add channel time_map channel_map
    ) ChannelMap.empty input
  in
  (* let ends = List.map (fun vod -> vod.API.stop) vods |> List.sort compare |> List.unique  in *)
  let last_index = List.length begins + 1 + 4 in
  let table = Array.make_matrix last_index (List.length channels + 1) empty in
  let _ = List.fold_left (fun i channel -> table.(0).(i) <- text @@ channel ^ " "; (i + 1)) 1 channels in
  let _ = List.fold_left (fun i time ->
      table.(i + 1).(0) <- text (ISO8601.Permissive.string_of_datetime time ^ " ");
      List.fold_left (fun x channel ->
          ( match ChannelMap.find channel programs_by_channel_and_begin |> TimeMap.find time with
            | exception Not_found -> ()
            | vod ->
              let next_time =
                try Some (
                    let (_, _, following) = ChannelMap.find channel programs_by_channel_and_begin |> TimeMap.split time in
                    fst (TimeMap.min_binding following)
                  )
                with Not_found -> None
              in
              let next_index =
                match next_time with
                | None -> last_index - 1
                | Some next_time ->
                  try fst @@ List.findi (fun _ time -> time = next_time) begins
                  with Not_found -> last_index - 1
              in
              let vertical_space = min 5 (next_index - i) in
              let title =
                match title_for vod, vod.API.pid with
                | None, None -> "???"
                | None, Some pid -> pid
                | Some title, _ -> title
              in
              let title = "* " ^ title in
              let part_length = max 5 ((Text.length title + vertical_space - 1) / vertical_space) in
              let parts = split_string_to_chunks part_length title in
              List.fold_left
                (fun part_index part ->
                   table.(i + 1 + part_index).(x) <- text part;
                   (part_index + 1)
                ) 0 parts |> ignore
          );
          (x + 1)
        ) 1 channels |> ignore;
      (i + 1)
    ) 0 begins in
  let channels_box = `Hlist (`Empty::List.map (fun ch -> `Text ch) channels) in
  (* let box = `Vlist (channels_box :: List.map (fun time -> `Text (ISO8601.Permissive.string_of_datetime time)) begins) in *)
  let grid_box = grid ~bars:false table in
  Printf.printf "%s" (to_string grid_box);
  (* Printf.printf "%s" (Simple.to_string channels_box); *)
  ()

let output_program_list input =
  input |> List.iter @@ fun (channel, vods) ->
  Printf.printf "%s:\n" channel;
  vods |> List.iter @@ fun vod ->
  Printf.printf "  %s %s [%s] %s\n"
    (ISO8601.Permissive.string_of_datetime vod.API.start)
    (Option.default "???" vod.API.pid)
    (if vod.API.downloadFormats <> []
     then "OK"
     else "--"
    )
    (Option.default "???" (title_for vod))

let optionally_filter_channels channels range_response =
  match channels with
  | None -> range_response
  | Some channels -> List.filter (fun (channel, _) -> List.mem channel channels) range_response

let vod_name_matches pattern _channel vod =
  match title_for vod with
  | None -> false
  | Some title -> Re.execp pattern title

let vod_description_matches pattern _channel vod =
  match subtitle_for vod with
  | None -> false
  | Some title -> Re.execp pattern title

let filter_empty_vods = List.filter (function (_channel, []) -> false | _ -> true)

let filter_vods f range_response =
  range_response |>
  List.map (fun (channel, vods) -> (channel, List.filter (fun vod -> f channel vod) vods)) |>
  filter_empty_vods

let optionally_filter_name name_filter range_response =
  match name_filter with
  | None -> range_response
  | Some name_filter ->
    let re = Re.str name_filter |> Re.no_case |> Re.compile in
    filter_vods (vod_name_matches re) range_response

let optionally_filter_description description_filter range_response =
  match description_filter with
  | None -> range_response
  | Some description_filter ->
    let re = Re.str description_filter |> Re.no_case |> Re.compile in
    filter_vods (vod_description_matches re) range_response

let cmd_list env =
  let range common from_ to_ load_file save_file channels name_filter description_filter =
    let channels =
      (* ensure there is never an empty channel filter *)
      match channels with
      | [] -> None
      | xs -> Some channels
    in
    let%lwt response =
      match load_file, from_, to_ with
      | None, Some from_, Some to_ -> (
          match%lwt Endpoints.epg_range common.Common.c_session (renegotiate_session common) { API.from_; to_ } with
          | Endpoints.Invalid_response ->
            Printf.printf "Failed to receive response (invalid response)\n%!";
            return None
          | Endpoints.Error error ->
            Printf.printf "Failed to receive response: %s\n%!" error;
            return None
          | Endpoints.Ok response ->
            return (Some response)
        )
      | Some file, None, None -> (
          let input = File.with_file_in file IO.read_all |> Yojson.Safe.from_string |> API.range_response_of_yojson in
          ( match input with
            | `Error error ->
              Printf.printf "Failed to load response: %s\n" error;
              return None
            | `Ok input ->
              return (Some input)
          );
        )
      | _, _, _ ->
        Printf.printf "Need to provide either time range or --load\n";
        return None
    in
    match response with
    | None -> return ()
    | Some response ->
      (* saving is always unfiltered, to allow refiltering it. bad choice? *)
      ( match save_file with
        | None -> ()
        | Some file ->
          File.with_file_out file @@ fun io ->
          Printf.fprintf io "%s" (API.range_response_to_yojson response |> Yojson.Safe.pretty_to_string)
      );
      response
      |> optionally_filter_channels channels
      |> optionally_filter_name name_filter
      |> optionally_filter_description description_filter
      |> output_program_list;
      return ()
  in
  let doc = "List vods from given time range" in
  Term.(pure range $ common_opts_t env $ begin_t $ end_t $ load_file_t $ save_file_t $ channels_t $ name_filter_t $ description_filter_t),
  Term.info "list" ~doc

let cmd_cache env =
  let cache common =
    interactive_request common Endpoints.cache_get common.Common.c_session () @@
    fun response -> API.show_cache_response response
  in
  let doc = "List cache servers" in
  Term.(pure cache $ common_opts_t env),
  Term.info "cache" ~doc

(* useless currently. what are the parameters to the request? *)
let cmd_vod_url env =
  let vod_url common pid =
    match%lwt Endpoints.epg_info common.Common.c_session (renegotiate_session common) { API.pid } with
    | Endpoints.Invalid_response ->
      Printf.printf "Invalid response from the server\n";
      return ()
    | Endpoints.Error error ->
      Printf.printf "Sorry, no such programm id could be found: %s\n" error;
      return ()
    | Endpoints.Ok ({ recordpath = Some recordpath }) ->
      let request = {
        API.pid;
        path = recordpath;
        file = "highest.mp4";
      } in
      interactive_request common Endpoints.client_vod_getUrl common.Common.c_session request @@
      fun response -> API.show_json_response response
    | Endpoints.Ok _ ->
      Printf.printf "Sorry, no recording for the program id could be found\n";
      return ()
  in
  let doc = "NOT WORKING: Retrieve the URL of a program" in
  Term.(pure vod_url $ common_opts_t env $ pid_t),
  Term.info "vod-url" ~doc

let vod_url common session cache_server pid format quality =
  match%lwt Endpoints.epg_info session (renegotiate_session common) { API.pid } with
  | Endpoints.Error error ->
    Printf.eprintf "Error: %s\n%!" error;
    return None
  | Endpoints.Invalid_response ->
    Printf.eprintf "Invalid response\n%!";
    return None
  | Endpoints.Ok ({ recordpath = Some recordpath } as info) -> (
      let filter_by_format format = List.filter (fun (f, _) -> f = format) in
      let filter_by_quality quality = List.filter (fun (_, q) -> q = quality) in
      let preference =
        match format, quality with
        | None, None -> format_quality_preference
        | Some format, None -> filter_by_format format format_quality_preference
        | None, Some quality -> filter_by_quality quality format_quality_preference
        | Some format, Some quality -> filter_by_quality quality format_quality_preference |> filter_by_format format
      in
      match format_quality_for preference info with
      | None ->
        (* Printf.printf "Sorry, there is no match for required format/quality\n%!"; *)
        return None
      | Some (format, quality) ->
        let title = Option.default "file" @@ title_for info in
        let title = Re.replace_string (Re.compile @@ Re_pcre.re "[/ ]") ~by:"_" title in
        return @@ Endpoints.download_url cache_server session format quality info title
    )
  | Endpoints.Ok _ -> return None

let cmd_url env =
  let url common cache_server pid format quality =
    match%lwt vod_url common common.Common.c_session cache_server pid format quality with
    | None ->
      Printf.printf "Sorry, no such programm id could be found\n%!";
      return ()
    | Some url ->
      Printf.printf "%s\n%!" url;
      return ()
  in
  let doc = "Retrieve the URL of a program" in
  Term.(pure url $ common_opts_t env $ cache_server_t env.Common.e_persist $ pid_t $ format_t $ quality_t),
  Term.info "url" ~doc

let download_url url =
  let filename = Uri.path url |> Re.replace ~f:(const "") (Re_pcre.re "^(.*/)"|> Re.compile) in
  let filename_tmp = filename ^ ".partial" in
  let headers = Cohttp.Header.of_list [] in
  let%lwt (response, body) = Cohttp_lwt_unix.Client.get ~headers url in
  let headers = response.headers in
  let total_length = Option.map Int64.of_string @@ Cohttp.Header.get headers "Content-Length" in
  let stream = Cohttp_lwt_body.to_stream body in
  Printf.printf "Saving to %s\n%!" filename_tmp;
  let file = open_out filename_tmp in
  let%lwt _ = Lwt_stream.fold_s (
      fun str received_bytes ->
        let bytes = String.length str in
        let received_bytes = Int64.(add received_bytes (of_int bytes)) in
        Printf.printf "%Ld/%s\r%!" received_bytes (match total_length with None -> "unknown" | Some b -> Int64.to_string b);
        output_string file str;
        return received_bytes
    ) stream 0L
  in
  close_out file;
  Printf.printf "Renaming %s->%s\n%!" filename_tmp filename;
  Sys.rename filename_tmp filename;
  return ()

let cmd_download env =
  let command common cache_server pids format quality =
    pids |> Lwt_list.iter_s @@ fun pid ->
    match%lwt vod_url common common.Common.c_session cache_server pid format quality with
    | None ->
      Printf.printf "Sorry, no such programm id could be found\n%!";
      return ()
    | Some url ->
      Printf.printf "%s\n%!" url;
      download_url (Uri.of_string url)
  in
  let doc = "Retrieve a program" in
  Term.(pure command $ common_opts_t env $ cache_server_t env.Common.e_persist $ pids_t $ format_t $ quality_t),
  Term.info "download" ~doc

let cmd_epg_info env =
  let epg_info common pid =
    let request = {
      API.pid = pid;
    } in
    interactive_request common Endpoints.epg_info common.Common.c_session request @@
    API.show_epg_info_response
  in
  let doc = "Retrieve info about a program" in
  Term.(pure epg_info $ common_opts_t env $ pid_t),
  Term.info "info" ~doc

let main () =
  let subcommands = [
    cmd_checkSession;
    cmd_login;
    cmd_settings;
    cmd_list;
    cmd_cache;
    cmd_epg_info;
    cmd_vod_url;
    cmd_url;
    cmd_download;
  ] in
  let env = { Common.e_persist = Persist.load_persist () } in
  match Term.eval_choice (default_prompt env) (List.map (fun x -> x env) subcommands)
  with
  | `Error _   -> exit 1
  | `Version | `Help -> exit 0
  | `Ok result ->
    Lwt_unix.run result;
    Persist.save_persist env.Common.e_persist;
    exit 0

let _ =
  if not !(Sys.interactive) then
    main ()
