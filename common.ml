type environment = {
  e_persist    : Persist.t;     (* for persisting login and session *)
  e_ep_context : Endpoints.context;
}

type common = {
  c_session : API.session_token; (* session id is kept here *)
  c_env     : environment;       (* auxiliary environment data (ie. loaded configuration, location of configuration) *)
}
