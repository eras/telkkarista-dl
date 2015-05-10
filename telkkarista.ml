module Text' = Text
open Batteries
module Text = Text'
open Lwt.Infix
open Cmdliner

let program_name = "telkkarista"
let version = "0.1"

let return = Lwt.return

let interactive_request endpoint session arg show =
  let%lwt response = endpoint session arg in
  match response with
  | None ->
    Printf.printf "There was an error\n%!";
    return ()
  | Some response ->
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
  let doc = "User name (email). This is required." in
  req_optional Arg.string (Option.map fst (Persist.get_email_password persist)) (Arg.info ["u"; "user"] ~docv:"EMAIL" ~doc)

let password_t persist =
  let doc = "Password. This is required." in
  req_optional Arg.string (Option.map snd (Persist.get_email_password persist)) (Arg.info ["p"; "pass"] ~docv:"PASSWORD" ~doc)

let pid_t =
  let doc = "Program ID. This is required." in
  req_optional Arg.string None (Arg.info ["p"; "pid"] ~docv:"PID" ~doc)

let format_t =
  let doc = "Format for download. This is required." in
  Arg.(value & opt (some string) None & (Arg.info ["f"; "format"] ~docv:"FORMAT" ~doc))

let quality_t =
  let doc = "Quality for download. This is required." in
  Arg.(value & opt (some string) None & (Arg.info ["q"; "quality"] ~docv:"QUALITY" ~doc))

let best_cache persist =
  match
    Persist.get_cache_servers persist |>
    List.sort (fun (_, a) (_, b) -> compare b.API.mbit a.API.mbit)
  with
  | (fastest, _)::_ -> Some fastest
  | _ -> None

let cache_server_t persist =
  let doc = "Cache server for download. This is required." in
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
let lwt6 f a b c d e g = Lwt_unix.run (f a b c d e g)
let lwt7 f a b c d e g h = Lwt_unix.run (f a b c d e g h)
let lwt8 f a b c d e g h i = Lwt_unix.run (f a b c d e g h i)
let lwt9 f a b c d e g h i j = Lwt_unix.run (f a b c d e g h i j)

let cmd_checkSession env =
  let checkSession common =
    interactive_request Endpoints.checkSession_request common.Common.c_session () @@
    fun response -> response.API._id
  in
  let doc = "Check session status" in
  Term.(pure (lwt1 checkSession) $ common_opts_t env),
  Term.info "checkSession" ~doc

let cmd_login env =
  let login common email password =
    interactive_request Endpoints.login_request () { API.email; password } @@
    fun token ->
    Persist.set_email_password env.Common.e_persist email password;
    Persist.set_session env.Common.e_persist token;
    token
  in
  let doc = "Log into the service" in
  Term.(pure (lwt3 login) $ common_opts_t env $ username_t env.Common.e_persist $ password_t env.Common.e_persist),
  Term.info "login" ~doc

let cmd_settings env =
  let settings common =
    interactive_request Endpoints.user_settings_request common.Common.c_session () @@
    fun settings ->
    List.iter
      (function
        | API.Speedtests speedtests -> Persist.set_cache_servers env.Common.e_persist speedtests
        | Other _ -> ())
      settings;
    API.show_user_settings_response settings
  in
  let doc = "Retrieve settings" in
  Term.(pure (lwt1 settings) $ common_opts_t env),
  Term.info "settings" ~doc

module TimeMap = Map.Make (struct type t = float let compare = compare end)
module ChannelMap = Map.Make (struct type t = API.channel let compare = compare end)

let format_quality_preference = [("ts", "highest"); ("ts", "hi"); ("mp4", "highest"); ("mp4", "hi")]

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

let format_quality_for preference vod =
  let formats =
    vod.API.downloadFormats
    |> List.map (fun (format, quality) -> List.map (fun q -> (format, q)) quality)
    |> List.concat
  in
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
  Printf.printf "  %s %s %s\n"
    (ISO8601.Permissive.string_of_datetime vod.API.start)
    (Option.default "???" vod.API.pid)
    (Option.default "???" (title_for vod))

let cmd_list env =
  let range common from_ to_ load_file save_file channels =
    match load_file, from_, to_ with
    | None, Some from_, Some to_ -> (
        match%lwt Endpoints.range_request common.Common.c_session { API.from_; to_ } with
        | None ->
          Printf.printf "Failed to receive response\n%!";
          return ()
        | Some response ->
          ( match save_file with
            | None -> ()
            | Some file ->
              File.with_file_out file @@ fun io ->
              Printf.fprintf io "%s" (API.range_response_to_yojson response |> Yojson.Safe.pretty_to_string)
          );
          (* API.show_range_response response *)
          output_program_list response;
          return ()
      )
    | Some file, None, None -> (
        let input = File.with_file_in file IO.read_all |> Yojson.Safe.from_string |> API.range_response_of_yojson in
        ( match input with
          | `Error error -> Printf.printf "Failed to load response: %s\n" error
          | `Ok input ->
            let input =
              if channels = []
              then input
              else List.filter (fun (channel, _) -> List.mem channel channels) input
            in
            output_program_list input
        );
        return ()
      )
    | _, _, _ ->
      Printf.printf "Need to provide either time range or --load\n";
      return ()
  in
  let doc = "List vods from given time range" in
  Term.(pure (lwt6 range) $ common_opts_t env $ begin_t $ end_t $ load_file_t $ save_file_t $ channels_t),
  Term.info "list" ~doc

let cmd_cache env =
  let cache common =
    interactive_request Endpoints.cache_request common.Common.c_session () @@
    fun response -> API.show_cache_response response
  in
  let doc = "List cache servers" in
  Term.(pure (lwt1 cache) $ common_opts_t env),
  Term.info "cache" ~doc

(* useless currently. what are the parameters to the request? *)
let cmd_vod_url env =
  let vod_url common pid =
    match%lwt Endpoints.epg_info_request common.Common.c_session { API.pid } with
    | None ->
      Printf.printf "Sorry, no such programm id could be found\n";
      return ()
    | Some ({ recordpath = Some recordpath }) ->
      let request = {
        API.pid;
        path = recordpath;
        file = "highest.mp4";
      } in
      interactive_request Endpoints.client_vod_getUrl_request common.Common.c_session request @@
      fun response -> API.show_json_response response
    | Some _ ->
      Printf.printf "Sorry, no recording for the program id could be found\n";
      return ()
  in
  let doc = "NOT WORKING: Retrieve the URL of a program" in
  Term.(pure (lwt2 vod_url) $ common_opts_t env $ pid_t),
  Term.info "vod-url" ~doc

let cmd_url env =
  let url common cache_server pid format quality =
    match%lwt Endpoints.epg_info_request common.Common.c_session { API.pid } with
    | None ->
      Printf.printf "Sorry, no such programm id could be found\n";
      return ()
    | Some ({ recordpath = Some recordpath } as info) -> (
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
          Printf.printf "Sorry, there is no match for required format/quality\n%!";
          return ()
        | Some (format, quality) ->
          let title = Option.default "file" @@ title_for info in
          let title = Re.replace_string (Re.compile @@ Re_pcre.re "[/ ]") ~by:"_" title in
          let url = Endpoints.download_url cache_server common.Common.c_session format quality info title in
          Printf.printf "%s\n%!" (Option.default "not available" url);
          return ()
      )
    | Some _ ->
      Printf.printf "Sorry, no recording for the program id could be found\n";
      return ()
  in
  let doc = "Retrieve the URL of a program" in
  Term.(pure (lwt5 url) $ common_opts_t env $ cache_server_t env.Common.e_persist $ pid_t $ format_t $ quality_t),
  Term.info "url" ~doc
  

let cmd_epg_info env =
  let epg_info common pid =
    let request = {
      API.pid = pid;
    } in
    interactive_request Endpoints.epg_info_request common.Common.c_session request @@
    API.show_epg_info_response
  in
  let doc = "Retrieve info about a program" in
  Term.(pure (lwt2 epg_info) $ common_opts_t env $ pid_t),
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
  ] in
  let env = { Common.e_persist = Persist.load_persist () } in
  match Term.eval_choice (default_prompt env) (List.map (fun x -> x env) subcommands)
  with
  | `Error _ -> exit 1
  | _        ->
    Persist.save_persist env.Common.e_persist;
    exit 0

let _ =
  if not !(Sys.interactive) then
    main ()
