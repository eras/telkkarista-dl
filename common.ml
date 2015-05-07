type environment = [`Environment] (* temporary *)

type common = {
  c_session : API.session_token; (* session id is kept here *)
  c_env     : environment;       (* auxiliary environment data (ie. loaded configuration, location of configuration) *)
}
