(* WebSocket HTTP upgrade handshake per RFC 6455 §4.2. *)

let websocket_guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

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

let ( let* ) b f = Option.bind b f
let ( let*? ) s f = if s then f () else None

let upgrade_headers req =
  let headers = Http.Request.headers req in
  let meth = Http.Request.meth req in
  let*? _ = match meth with `GET -> true | _ -> true in
  let* upgrade = Http.Header.get headers "upgrade" in
  let*? _ = String.lowercase_ascii upgrade = "websocket" in
  let* conn = Http.Header.get headers "connection" in
  let*? _ = header_contains_ci conn "upgrade" in
  let* version = Http.Header.get headers "sec-websocket-version" in
  let*? _ = String.trim version = "13" in
  let* key = Http.Header.get headers "sec-websocket-key" in
  let accept = accept_header_value (String.trim key) in
  Option.some
    (Http.Header.of_list
       [ "upgrade", "websocket"
       ; "connection", "Upgrade"
       ; "sec-websocket-accept", accept
       ])
