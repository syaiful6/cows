module Logs =
  (val Logs.src_log
         (Logs.Src.create "cows.connections" ~doc:"Cows connection module"))

module Eio_reader = struct
  type t = Eio.Buf_read.t

  let uint8 = Eio.Buf_read.uint8
  let uint16 = Eio.Buf_read.BE.uint16
  let uint64 = Eio.Buf_read.BE.uint64
  let take = Eio.Buf_read.take
end

module Eio_writer = struct
  type t = Eio.Buf_write.t

  let uint8 = Eio.Buf_write.uint8
  let uint16 = Eio.Buf_write.BE.uint16
  let uint64 = Eio.Buf_write.BE.uint64
  let string oc s = Eio.Buf_write.string oc s
end

type role =
  | Server
  | Client

exception Connection_closed
exception Close_request of int * string
exception Protocol_error of string

module Message = struct
  type control =
    [ `Ping of string
    | `Pong of string
    | `Close of int * string
    ]

  type data =
    [ `Text of string
    | `Binary of string
    ]

  type t =
    [ control
    | data
    ]
end

type t =
  { ic : Eio.Buf_read.t
  ; oc : Eio.Buf_write.t
  ; send_mutex : Eio.Mutex.t
  ; role : role
  ; size_limit : Frame.Size_limit.t
  ; mutable frag_opcode : [ `Text | `Binary ] option
  ; mutable frag_buf : Buffer.t option
  }

let create ?(size_limit = Frame.Size_limit.default_max_frame) ~role ic oc =
  { ic
  ; oc
  ; send_mutex = Eio.Mutex.create ()
  ; role
  ; size_limit
  ; frag_opcode = None
  ; frag_buf = None
  }

let make_frame ?(fin = true) opcode content =
  Frame.{ fin; rsv1 = false; rsv2 = false; rsv3 = false; opcode; content }

let fresh_mask_key () =
  let lo = Random.bits () in
  let hi = Random.bits () land 0x3 in
  lo lor (hi lsl 30)

let write_frame conn frame =
  Eio.Mutex.use_rw ~protect:true conn.send_mutex (fun () ->
    match conn.role with
    | Server -> Frame.serialize (module Eio_writer) conn.oc frame
    | Client ->
      let key = fresh_mask_key () in
      Frame.serialize ~key (module Eio_writer) conn.oc frame)

let create_close_frame ?(code = 1000) ?(reason = "") () =
  (* 1005 (no status received) must never appear on the wire per RFC 6455
     §7.4.1; substitute 1000 (normal closure). *)
  let code = if code = 1005 then 1000 else code in
  let n = String.length reason in
  let buf = Bytes.create (2 + n) in
  Bytes.set buf 0 (Char.chr ((code lsr 8) land 0xff));
  Bytes.set buf 1 (Char.chr (code land 0xff));
  Bytes.blit_string reason 0 buf 2 n;
  make_frame `Close (Bytes.unsafe_to_string buf)

let send conn msg =
  let is_close, frame =
    match msg with
    | `Text content -> false, make_frame `Text content
    | `Binary content -> false, make_frame `Binary content
    | `Ping content -> false, make_frame `Ping content
    | `Pong content -> false, make_frame `Pong content
    | `Close (code, reason) -> true, create_close_frame ~code ~reason ()
  in
  write_frame conn frame;
  if is_close
  then begin
    Eio.Buf_write.flush conn.oc
  end

let close ?(code = 1000) ?(reason = "") conn = send conn (`Close (code, reason))

let is_valid_close_code code =
  (code >= 1000 && code <= 1003)
  || (code >= 1007 && code <= 1011)
  || (code >= 3000 && code <= 4999)

let fail_with_close conn msg =
  (try
     write_frame conn (create_close_frame ~code:1002 ());
     Eio.Buf_write.flush conn.oc
   with
  | _ -> ());
  raise (Protocol_error msg)

let frame_error_to_string = function
  | Frame.Reserved_bits_set -> "reserved bits set"
  | Frame.Reserved_opcode i -> Printf.sprintf "reserved opcode %d" i
  | Frame.Control_frame_fragmented -> "control frame fragmented"
  | Frame.Control_frame_too_large -> "control frame too large"
  | Frame.Frame_too_large -> "frame too large"
  | Frame.Insufficient_data -> "insufficient data"

let rec recv conn =
  match Frame.parse ~size_limit:conn.size_limit (module Eio_reader) conn.ic with
  | Error Frame.Insufficient_data ->
    Logs.debug (fun f -> f "Connection closed: insufficient data");
    raise Connection_closed
  | Error e -> raise (Protocol_error (frame_error_to_string e))
  | Ok frame ->
    (match frame.Frame.opcode with
    | `Close ->
      let content = frame.Frame.content in
      let n = String.length content in
      if n = 1
      then fail_with_close conn "close frame with 1-byte payload"
      else if n = 0
      then `Close (1005, "")
      else
        let code =
          (Char.code (String.unsafe_get content 0) lsl 8)
          lor Char.code (String.unsafe_get content 1)
        in
        let reason = String.sub content 2 (n - 2) in
        if not (is_valid_close_code code)
        then fail_with_close conn (Printf.sprintf "invalid close code %d" code)
        else if not (Utf8.is_valid reason)
        then fail_with_close conn "invalid UTF-8 in close reason"
        else `Close (code, reason)
    | `Ping -> `Ping frame.Frame.content
    | `Pong -> `Pong frame.Frame.content
    | (`Text | `Binary) as opcode ->
      if Option.is_some conn.frag_opcode
      then raise (Protocol_error "new data frame during fragmented message")
      else if frame.Frame.fin
      then
        match opcode with
        | `Text ->
          if not (Utf8.is_valid frame.Frame.content)
          then raise (Protocol_error "invalid UTF-8 in text frame");
          `Text frame.Frame.content
        | `Binary -> `Binary frame.Frame.content
      else begin
        (* First fragment: stash opcode and start accumulator. *)
        conn.frag_opcode <- Some opcode;
        let buf = Buffer.create (String.length frame.Frame.content) in
        Buffer.add_string buf frame.Frame.content;
        conn.frag_buf <- Some buf;
        recv conn
      end
    | `Continuation ->
      (match conn.frag_opcode, conn.frag_buf with
      | None, _ | Some _, None ->
        raise (Protocol_error "unexpected continuation frame")
      | Some opcode, Some buf ->
        Buffer.add_string buf frame.Frame.content;
        if frame.Frame.fin
        then begin
          let content = Buffer.contents buf in
          conn.frag_opcode <- None;
          conn.frag_buf <- None;
          match opcode with
          | `Text ->
            if not (Utf8.is_valid content)
            then raise (Protocol_error "invalid UTF-8 in text frame");
            `Text content
          | `Binary -> `Binary content
        end
        else recv conn)
    | `Ctrl _ | `Non_ctrl _ -> raise (Protocol_error "unknown opcode"))

let upgrade ?size_limit req handler =
  match Handshake.upgrade_headers req with
  | Error _ ->
    `Response
      (Cohttp_eio.Server.respond_string
         ~status:`Bad_request
         ~body:"Bad WebSocket handshake"
         ())
  | Ok resp_headers ->
    let response =
      Http.Response.make ~status:`Switching_protocols ~headers:resp_headers ()
    in
    `Expert
      ( response
      , fun ic oc ->
          let conn = create ?size_limit ~role:Server ic oc in
          (* wrap user handler so we guarantee that any exceptions are caught,
             and we correctly close the connection, users still able to catch
             those exceptions *)
          try handler conn with
          | Connection_closed ->
            Logs.debug (fun f -> f "Connection closed by peer")
          | Protocol_error msg ->
            Logs.info (fun f -> f "Protocol error: %s" msg) )
