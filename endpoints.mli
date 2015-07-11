type 'session update_session = unit -> 'session Lwt.t
type 'a value = Ok of 'a | Invalid_response | Error of string
type context = { c_debug : bool }
type ('session, 'request, 'response) result = 'session -> 'session update_session -> 'request -> context -> 'response value Lwt.t
    
val version : int

val user_login : (unit, API.login_request, string) result
    
val user_checkSession : (API.session_token, unit, API.checkSession_response) result
    
val epg_range : (API.session_token, API.range_request, (string * API.vod list) list) result
    
val cache_get : (API.session_token, unit, API.cache list) result
    
val client_vod_getUrl : (API.session_token, API.client_vod_getUrl_request, Yojson.Safe.json) result
    
val epg_info : (API.session_token, API.epg_info_request, API.vod) result

val epg_titles : (API.session_token, unit, API.epg_titles_response) result
    
val user_settings : (API.session_token, unit, API.user_settings_response) result

val download_url : string -> API.session_token -> string -> API.quality -> API.vod -> string -> string option
    
val news_get : (API.session_token, unit, API.news_get_response) result

val payment_getPackages : (API.session_token, unit, API.payment_getPackages_response) result
