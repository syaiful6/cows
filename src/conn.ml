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

type close_reason =
  | Clean of int * string
  | Abnormal

exception Connection_closed of close_reason
exception Protocol_error of string

type message =
  | Text of string
  | Binary of string
  | Ping of string
  | Pong of string

type t =
  { ic : Eio.Buf_read.t
  ; oc : Eio.Buf_write.t
  ; send_mutex : Eio.Mutex.t
  ; role : role
  ; mutable frag_opcode : [ `Text | `Binary ] option
  ; mutable frag_buf : Buffer.t option
  }

let create ~role ic oc =
  { ic
  ; oc
  ; send_mutex = Eio.Mutex.create ()
  ; role
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

let send conn msg =
  let frame =
    match msg with
    | Text content -> make_frame `Text content
    | Binary content -> make_frame `Binary content
    | Ping content -> make_frame `Ping content
    | Pong content -> make_frame `Pong content
  in
  write_frame conn frame

let close ?(code = 1000) ?(reason = "") conn =
  let n = String.length reason in
  let buf = Bytes.create (2 + n) in
  Bytes.set buf 0 (Char.chr ((code lsr 8) land 0xff));
  Bytes.set buf 1 (Char.chr (code land 0xff));
  Bytes.blit_string reason 0 buf 2 n;
  write_frame conn (make_frame `Close (Bytes.unsafe_to_string buf));
  (* Flush immediately so the Close frame reaches the peer before we return. *)
  Eio.Buf_write.flush conn.oc

let parse_close_payload content =
  if String.length content >= 2
  then
    let code =
      (Char.code (String.unsafe_get content 0) lsl 8)
      lor Char.code (String.unsafe_get content 1)
    in
    let reason = String.sub content 2 (String.length content - 2) in
    code, reason
  else 1005, ""

let frame_error_to_string = function
  | Frame.Reserved_bits_set -> "reserved bits set"
  | Frame.Reserved_opcode i -> Printf.sprintf "reserved opcode %d" i
  | Frame.Control_frame_fragmented -> "control frame fragmented"
  | Frame.Control_frame_too_large -> "control frame too large"
  | Frame.Frame_too_large -> "frame too large"
  | Frame.Insufficient_data -> "insufficient data"

let rec recv conn =
  match Frame.parse (module Eio_reader) conn.ic with
  | Error Frame.Insufficient_data -> raise (Connection_closed Abnormal)
  | Error e -> raise (Protocol_error (frame_error_to_string e))
  | Ok frame ->
    (match frame.Frame.opcode with
    | `Close ->
      let code, reason = parse_close_payload frame.Frame.content in
      (* Echo the Close frame and flush before raising so the peer sees it. *)
      (try
         write_frame conn (make_frame `Close frame.Frame.content);
         Eio.Buf_write.flush conn.oc
       with
      | _ -> ());
      raise (Connection_closed (Clean (code, reason)))
    | `Ping -> Ping frame.Frame.content
    | `Pong -> Pong frame.Frame.content
    | (`Text | `Binary) as opcode ->
      if frame.Frame.fin
      then
        match opcode with
        | `Text ->
          if not (Utf8.is_valid frame.Frame.content)
          then raise (Protocol_error "invalid UTF-8 in text frame");
          Text frame.Frame.content
        | `Binary -> Binary frame.Frame.content
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
            Text content
          | `Binary -> Binary content
        end
        else recv conn)
    | `Ctrl _ | `Non_ctrl _ -> raise (Protocol_error "unknown opcode"))

let upgrade req handler =
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
          let conn = create ~role:Server ic oc in
          handler conn )
