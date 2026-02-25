(* WebSocket HTTP upgrade handshake per RFC 6455 §4.2. *)

let websocket_guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

type error =
  | Not_a_get
  | Missing_upgrade_header
  | Missing_connection_header
  | Missing_key_header
  | Bad_version

let accept_header_value key =
  let input = key ^ websocket_guid in
  let digest = Digestif.SHA1.(digest_string input |> to_raw_string) in
  Base64.encode_string digest

(* Case-insensitive substring search for header values like "upgrade" in
   "keep-alive, Upgrade". *)
let header_contains_ci value token =
  let vl = String.lowercase_ascii value in
  let tl = String.lowercase_ascii token in
  let vlen = String.length vl in
  let tlen = String.length tl in
  if tlen = 0
  then true
  else if tlen > vlen
  then false
  else
    let found = ref false in
    for i = 0 to vlen - tlen do
      if String.sub vl i tlen = tl then found := true
    done;
    !found

let upgrade_headers req =
  let headers = Http.Request.headers req in
  let meth = Http.Request.meth req in
  if meth <> `GET
  then Error Not_a_get
  else
    match Http.Header.get headers "upgrade" with
    | None -> Error Missing_upgrade_header
    | Some upgrade when String.lowercase_ascii upgrade <> "websocket" ->
      Error Missing_upgrade_header
    | Some _ ->
      (match Http.Header.get headers "connection" with
      | None -> Error Missing_connection_header
      | Some conn when not (header_contains_ci conn "upgrade") ->
        Error Missing_connection_header
      | Some _ ->
        (match Http.Header.get headers "sec-websocket-version" with
        | Some v when String.trim v <> "13" -> Error Bad_version
        | None -> Error Bad_version
        | Some _ ->
          (match Http.Header.get headers "sec-websocket-key" with
          | None -> Error Missing_key_header
          | Some key ->
            let accept = accept_header_value (String.trim key) in
            let resp_headers =
              Http.Header.of_list
                [ "upgrade", "websocket"
                ; "connection", "Upgrade"
                ; "sec-websocket-accept", accept
                ]
            in
            Ok resp_headers)))
