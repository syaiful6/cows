module Bytes_reader = struct
  type t =
    { buf : bytes
    ; mutable pos : int
    }

  let create s = { buf = Bytes.of_string s; pos = 0 }

  let uint8 r =
    if r.pos >= Bytes.length r.buf then raise End_of_file;
    let b = Char.code (Bytes.get r.buf r.pos) in
    r.pos <- r.pos + 1;
    b

  let uint16 r =
    let hi = uint8 r in
    let lo = uint8 r in
    (hi lsl 8) lor lo

  let uint64 r =
    let b0 = uint8 r in
    let b1 = uint8 r in
    let b2 = uint8 r in
    let b3 = uint8 r in
    let b4 = uint8 r in
    let b5 = uint8 r in
    let b6 = uint8 r in
    let b7 = uint8 r in
    let open Int64 in
    logor
      (shift_left (of_int b0) 56)
      (logor
         (shift_left (of_int b1) 48)
         (logor
            (shift_left (of_int b2) 40)
            (logor
               (shift_left (of_int b3) 32)
               (logor
                  (shift_left (of_int b4) 24)
                  (logor
                     (shift_left (of_int b5) 16)
                     (logor (shift_left (of_int b6) 8) (of_int b7)))))))

  let take n r =
    if r.pos + n > Bytes.length r.buf then raise End_of_file;
    let s = Bytes.sub_string r.buf r.pos n in
    r.pos <- r.pos + n;
    s
end

module Buffer_writer = struct
  type t = Buffer.t

  let uint8 buf b = Buffer.add_char buf (Char.chr (b land 0xff))

  let uint16 buf n =
    uint8 buf (n lsr 8);
    uint8 buf (n land 0xff)

  let uint64 buf n =
    for i = 7 downto 0 do
      uint8 buf Int64.(to_int (logand (shift_right_logical n (i * 8)) 0xffL))
    done

  let string buf s = Buffer.add_string buf s
end

let parse s =
  let r = Bytes_reader.create s in
  Cows.Frame.parse (module Bytes_reader) r

let parse_with_limit size_limit s =
  let r = Bytes_reader.create s in
  Cows.Frame.parse ~size_limit (module Bytes_reader) r

let serialize ?key frame =
  let buf = Buffer.create 16 in
  Cows.Frame.serialize ?key (module Buffer_writer) buf frame;
  Buffer.contents buf

let opcode =
  Alcotest.testable
    (fun ppf o -> Format.fprintf ppf "%d" (Cows.Opcode.to_int o))
    ( = )

let frame_error =
  Alcotest.testable
    (fun ppf _ -> Format.pp_print_string ppf "frame_error")
    ( = )

let mask_rfc_example () =
  let key = 0x37fa213d in
  let masked = Cows.Mask.apply key "Hello" in
  Alcotest.(check string) "RFC masked" "\x7f\x9f\x4d\x51\x58" masked

let mask_roundtrip () =
  let key = 0xdeadbeef in
  let payload = "The quick brown fox" in
  Alcotest.(check string)
    "roundtrip"
    payload
    (Cows.Mask.apply key (Cows.Mask.apply key payload))

let mask_empty () =
  Alcotest.(check string) "empty" "" (Cows.Mask.apply 0xdeadbeef "")

let parse_unmasked_hello () =
  match parse "\x81\x05Hello" with
  | Ok f ->
    Alcotest.(check bool) "fin" true f.fin;
    Alcotest.(check bool) "rsv1" false f.rsv1;
    Alcotest.(check opcode) "opcode" `Text f.opcode;
    Alcotest.(check string) "content" "Hello" f.content
  | Error _ -> Alcotest.fail "unexpected parse error"

let parse_masked_hello () =
  match parse "\x81\x85\x37\xfa\x21\x3d\x7f\x9f\x4d\x51\x58" with
  | Ok f ->
    Alcotest.(check bool) "fin" true f.fin;
    Alcotest.(check opcode) "opcode" `Text f.opcode;
    Alcotest.(check string) "content unmasked" "Hello" f.content
  | Error _ -> Alcotest.fail "unexpected parse error"

let parse_ping_no_payload () =
  match parse "\x89\x00" with
  | Ok f ->
    Alcotest.(check opcode) "opcode" `Ping f.opcode;
    Alcotest.(check string) "content" "" f.content
  | Error _ -> Alcotest.fail "unexpected parse error"

let parse_fin_false () =
  (* FIN=0, opcode=text: first fragment, valid *)
  match parse "\x01\x03Hel" with
  | Ok f ->
    Alcotest.(check bool) "fin false" false f.fin;
    Alcotest.(check string) "content" "Hel" f.content
  | Error _ -> Alcotest.fail "unexpected parse error"

let parse_reserved_bits () =
  (* RSV1 set: 0xC1 = 1100_0001 *)
  match parse "\xc1\x05Hello" with
  | Error e ->
    Alcotest.(check frame_error) "reserved" Cows.Frame.Reserved_bits_set e
  | Ok _ -> Alcotest.fail "expected error"

let parse_control_fragmented () =
  (* Ping with FIN=0: 0x09 = 0000_1001 *)
  match parse "\x09\x00" with
  | Error e ->
    Alcotest.(check frame_error)
      "fragmented ctrl"
      Cows.Frame.Control_frame_fragmented
      e
  | Ok _ -> Alcotest.fail "expected error"

let parse_control_too_large () =
  (* Ping with 126-byte payload would use ext length, but we can fake it: 0x89 =
     FIN+Ping, 0x7e = MASK=0, len=126 *)
  let hdr = "\x89\x7e\x00\x7e" in
  let payload = String.make 126 'x' in
  match parse (hdr ^ payload) with
  | Error e ->
    Alcotest.(check frame_error)
      "ctrl too large"
      Cows.Frame.Control_frame_too_large
      e
  | Ok _ -> Alcotest.fail "expected error"

let parse_insufficient_data () =
  match parse "\x81" with
  | Error e ->
    Alcotest.(check frame_error) "insufficient" Cows.Frame.Insufficient_data e
  | Ok _ -> Alcotest.fail "expected error"

let serialize_unmasked () =
  let frame =
    Cows.Frame.
      { fin = true
      ; rsv1 = false
      ; rsv2 = false
      ; rsv3 = false
      ; opcode = `Text
      ; content = "Hello"
      }
  in
  (* RFC §5.7 example 1 wire bytes *)
  Alcotest.(check string) "wire" "\x81\x05Hello" (serialize frame)

let roundtrip () =
  let frame =
    Cows.Frame.
      { fin = true
      ; rsv1 = false
      ; rsv2 = false
      ; rsv3 = false
      ; opcode = `Binary
      ; content = "arbitrary payload"
      }
  in
  match parse (serialize frame) with
  | Ok f -> Alcotest.(check string) "roundtrip" frame.content f.content
  | Error _ -> Alcotest.fail "roundtrip parse failed"

let roundtrip_masked () =
  let key = 0x37fa213d in
  let frame =
    Cows.Frame.
      { fin = true
      ; rsv1 = false
      ; rsv2 = false
      ; rsv3 = false
      ; opcode = `Text
      ; content = "Hello"
      }
  in
  match parse (serialize ~key frame) with
  | Ok f -> Alcotest.(check string) "masked roundtrip" frame.content f.content
  | Error _ -> Alcotest.fail "masked roundtrip parse failed"

(* RFC 6455 §4.2.2 example: known key → known accept value *)
let handshake_accept_value () =
  let key = "dGhlIHNhbXBsZSBub25jZQ==" in
  let accept = Cows.Private.Handshake.accept_header_value key in
  Alcotest.(check string) "accept" "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" accept

let make_upgrade_request headers = Http.Request.make ~meth:`GET ~headers "/ws"

let handshake_ok () =
  let headers =
    Http.Header.of_list
      [ "upgrade", "websocket"
      ; "connection", "keep-alive, Upgrade"
      ; "sec-websocket-version", "13"
      ; "sec-websocket-key", "dGhlIHNhbXBsZSBub25jZQ=="
      ]
  in
  let req = make_upgrade_request headers in
  match Cows.Private.Handshake.upgrade_headers req with
  | Some h ->
    Alcotest.(check string)
      "accept"
      "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
      (Http.Header.get h "sec-websocket-accept" |> Option.get)
  | None -> Alcotest.fail "expected headers"

let handshake_missing_key () =
  let headers =
    Http.Header.of_list
      [ "upgrade", "websocket"
      ; "connection", "Upgrade"
      ; "sec-websocket-version", "13"
      ]
  in
  let req = make_upgrade_request headers in
  match Cows.Private.Handshake.upgrade_headers req with
  | None -> ()
  | _ -> Alcotest.fail "expected Missing_key_header"

let handshake_bad_version () =
  let headers =
    Http.Header.of_list
      [ "upgrade", "websocket"
      ; "connection", "Upgrade"
      ; "sec-websocket-version", "8"
      ; "sec-websocket-key", "dGhlIHNhbXBsZSBub25jZQ=="
      ]
  in
  let req = make_upgrade_request headers in
  match Cows.Private.Handshake.upgrade_headers req with
  | None -> ()
  | _ -> Alcotest.fail "expected Bad_version"

let utf8_check label expect s =
  Alcotest.(check bool) label expect (Cows.Private.Utf8.is_valid s)

let utf8_valid () =
  utf8_check "empty" true "";
  utf8_check "ascii" true "Hello, World!";
  utf8_check "2-byte U+00A3 £" true "\xC2\xA3";
  utf8_check "3-byte U+20AC €" true "\xE2\x82\xAC";
  utf8_check "4-byte U+1F600" true "\xF0\x9F\x98\x80";
  utf8_check "U+0080 min 2-byte" true "\xC2\x80";
  utf8_check "U+07FF max 2-byte" true "\xDF\xBF";
  utf8_check "U+0800 min 3-byte" true "\xE0\xA0\x80";
  utf8_check "U+FFFD replacement" true "\xEF\xBF\xBD";
  utf8_check "U+D7FF just below surrogates" true "\xED\x9F\xBF";
  utf8_check "U+E000 just above surrogates" true "\xEE\x80\x80";
  utf8_check "U+10000 min 4-byte" true "\xF0\x90\x80\x80";
  utf8_check "U+10FFFF max" true "\xF4\x8F\xBF\xBF"

let utf8_invalid () =
  (* Overlong encodings *)
  utf8_check "overlong 2-byte 0xC0 0x80" false "\xC0\x80";
  utf8_check "overlong 2-byte 0xC1 0xBF" false "\xC1\xBF";
  utf8_check "overlong 3-byte U+0000" false "\xE0\x80\x80";
  utf8_check "overlong 3-byte U+007F" false "\xE0\x9F\xBF";
  utf8_check "overlong 4-byte U+0000" false "\xF0\x80\x80\x80";
  (* Surrogates U+D800..U+DFFF *)
  utf8_check "surrogate U+D800" false "\xED\xA0\x80";
  utf8_check "surrogate U+DFFF" false "\xED\xBF\xBF";
  (* Above U+10FFFF *)
  utf8_check "U+110000" false "\xF4\x90\x80\x80";
  utf8_check "lead 0xF5" false "\xF5\x80\x80\x80";
  (* Continuation byte as lead *)
  utf8_check "lone continuation 0x80" false "\x80";
  utf8_check "lone continuation 0xBF" false "\xBF";
  (* Invalid lead bytes *)
  utf8_check "lead 0xFE" false "\xFE";
  utf8_check "lead 0xFF" false "\xFF";
  (* Truncated sequences *)
  utf8_check "truncated 2-byte" false "\xC2";
  utf8_check "truncated 3-byte after 1" false "\xE2\x82";
  utf8_check "truncated 4-byte after 2" false "\xF0\x9F\x98";
  (* Non-continuation in sequence *)
  utf8_check "2-byte bad cont" false "\xC2\x20";
  utf8_check "3-byte bad cont2" false "\xE2\x82\x20"

(* FIN + Binary opcode header byte *)
let binary_hdr = '\x82'

(* size limit: small (len7 <= 125) encoding exceeds limit *)
let size_limit_exceeded_len7 () =
  (* limit=3, frame payload len=5; error raised before payload is read *)
  let limit = `Size_limit 3L in
  match parse_with_limit limit (String.make 1 binary_hdr ^ "\x05") with
  | Error e ->
    Alcotest.(check frame_error) "len7 exceeded" Cows.Frame.Frame_too_large e
  | Ok _ -> Alcotest.fail "expected Frame_too_large"

let size_limit_exceeded_uint16 () =
  (* limit=100, frame payload len=200 encoded as ext uint16 *)
  let limit = `Size_limit 100L in
  match parse_with_limit limit (String.make 1 binary_hdr ^ "\x7e\x00\xc8") with
  | Error e ->
    Alcotest.(check frame_error) "uint16 exceeded" Cows.Frame.Frame_too_large e
  | Ok _ -> Alcotest.fail "expected Frame_too_large"

let size_limit_exceeded_uint64 () =
  (* limit=100, frame payload len=65536 encoded as ext uint64 *)
  let limit = `Size_limit 100L in
  match
    parse_with_limit
      limit
      (String.make 1 binary_hdr ^ "\x7f\x00\x00\x00\x00\x00\x01\x00\x00")
  with
  | Error e ->
    Alcotest.(check frame_error) "uint64 exceeded" Cows.Frame.Frame_too_large e
  | Ok _ -> Alcotest.fail "expected Frame_too_large"

let size_limit_at_boundary () =
  let limit = `Size_limit 5L in
  match parse_with_limit limit (String.make 1 binary_hdr ^ "\x05Hello") with
  | Ok f -> Alcotest.(check string) "content" "Hello" f.content
  | Error _ -> Alcotest.fail "frame at limit should be accepted"

let size_limit_no_limit () =
  let payload = String.make 200 'x' in
  match
    parse_with_limit
      `No_size_limit
      (String.make 1 binary_hdr ^ "\x7e\x00\xc8" ^ payload)
  with
  | Ok f -> Alcotest.(check string) "content" payload f.content
  | Error _ -> Alcotest.fail "no-limit parse failed"

let () =
  Alcotest.run
    "cows Test"
    [ ( "Mask"
      , [ Alcotest.test_case "RFC 6455 example" `Quick mask_rfc_example
        ; Alcotest.test_case "roundtrip" `Quick mask_roundtrip
        ; Alcotest.test_case "empty payload" `Quick mask_empty
        ] )
    ; ( "Frame.parse"
      , [ Alcotest.test_case "unmasked hello" `Quick parse_unmasked_hello
        ; Alcotest.test_case "masked hello" `Quick parse_masked_hello
        ; Alcotest.test_case "ping no payload" `Quick parse_ping_no_payload
        ; Alcotest.test_case "fin=false first fragment" `Quick parse_fin_false
        ] )
    ; ( "Frame.parse errors"
      , [ Alcotest.test_case "reserved bits" `Quick parse_reserved_bits
        ; Alcotest.test_case
            "control fragmented"
            `Quick
            parse_control_fragmented
        ; Alcotest.test_case "control too large" `Quick parse_control_too_large
        ; Alcotest.test_case "insufficient data" `Quick parse_insufficient_data
        ] )
    ; ( "Frame.parse size_limit"
      , [ Alcotest.test_case
            "exceeded len7 encoding"
            `Quick
            size_limit_exceeded_len7
        ; Alcotest.test_case
            "exceeded uint16 encoding"
            `Quick
            size_limit_exceeded_uint16
        ; Alcotest.test_case
            "exceeded uint64 encoding"
            `Quick
            size_limit_exceeded_uint64
        ; Alcotest.test_case
            "at boundary accepted"
            `Quick
            size_limit_at_boundary
        ; Alcotest.test_case "no limit accepts large" `Quick size_limit_no_limit
        ] )
    ; ( "Frame.serialize"
      , [ Alcotest.test_case "unmasked wire bytes" `Quick serialize_unmasked
        ; Alcotest.test_case "roundtrip" `Quick roundtrip
        ; Alcotest.test_case "masked roundtrip" `Quick roundtrip_masked
        ] )
    ; ( "Handshake"
      , [ Alcotest.test_case "accept header value" `Quick handshake_accept_value
        ; Alcotest.test_case "valid upgrade" `Quick handshake_ok
        ; Alcotest.test_case "missing key" `Quick handshake_missing_key
        ; Alcotest.test_case "bad version" `Quick handshake_bad_version
        ] )
    ; ( "Utf8"
      , [ Alcotest.test_case "valid sequences" `Quick utf8_valid
        ; Alcotest.test_case "invalid sequences" `Quick utf8_invalid
        ] )
    ]
