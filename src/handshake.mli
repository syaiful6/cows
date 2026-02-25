type error =
  | Not_a_get
  | Missing_upgrade_header
  | Missing_connection_header
  | Missing_key_header
  | Bad_version

val accept_header_value : string -> string
val upgrade_headers : Http.Request.t -> (Http.Header.t, error) result
