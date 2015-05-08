type t

val persist_file : string
  
val load_persist : unit -> t

val save_persist : t -> unit

val set_email_password : t -> API.email -> API.password -> unit
val get_email_password : t -> (API.email * API.password) option

val set_session : t -> API.session_token -> unit
val get_session : t -> API.session_token option

val set_cache_servers : t -> API.speedtests -> unit
val get_cache_servers : t -> API.speedtests
