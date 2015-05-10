type ('session, 'request, 'response) result = 'session -> 'request -> 'response option Lwt.t
    
val version : int
val debug : bool

val login_request : (unit, API.login_request, string) result
    
val checkSession_request : (API.session_token, unit, API.checkSession_response) result
    
val range_request : (API.session_token, API.range_request, (string * API.vod list) list) result
    
val download_url : string -> API.session_token -> string -> API.quality -> API.vod -> string -> string option
    
val cache_request : (API.session_token, unit, API.cache list) result
    
val client_vod_getUrl_request : (API.session_token, API.client_vod_getUrl_request, Yojson.Safe.json) result
    
val epg_info_request : (API.session_token, API.epg_info_request, API.vod) result
    
val user_settings_request : (API.session_token, unit, API.user_setting list) result
