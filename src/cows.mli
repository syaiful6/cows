module Opcode : sig
  type t =
    [ `Continuation
    | `Text
    | `Binary
    | `Close
    | `Ping
    | `Pong
    | `Ctrl of int
    | `Non_ctrl of int
    ]

  val to_int : t -> int
  val of_int : int -> t
end

module Mask : sig
  val apply : int -> string -> string
end

module Frame : sig
  module type READER = sig
    type t

    val uint8 : t -> int
    val uint16 : t -> int
    val uint64 : t -> int64
    val take : int -> t -> string
  end

  module type WRITER = sig
    type t

    val uint8 : t -> int -> unit
    val uint16 : t -> int -> unit
    val uint64 : t -> int64 -> unit
    val string : t -> string -> unit
  end

  type error =
    | Insufficient_data
    | Reserved_bits_set
    | Reserved_opcode of int
    | Control_frame_fragmented
    | Control_frame_too_large
    | Frame_too_large

  type t =
    { fin : bool
    ; rsv1 : bool
    ; rsv2 : bool
    ; rsv3 : bool
    ; opcode : Opcode.t
    ; content : string
    }

  exception Parse_error of error

  val parse : (module READER with type t = 'r) -> 'r -> (t, error) result

  val serialize :
     ?key:int
    -> (module WRITER with type t = 'w)
    -> 'w
    -> t
    -> unit
end

module Private : sig
  module Utf8 = Utf8
  module Handshake = Handshake
end

type close_reason = Conn.close_reason =
  | Clean of int * string
  | Abnormal

exception Connection_closed of Conn.close_reason
exception Protocol_error of string

type message =
  | Text of string
  | Binary of string
  | Ping of string
  | Pong of string

type conn

val recv : conn -> message
val send : conn -> message -> unit
val close : ?code:int -> ?reason:string -> conn -> unit

val upgrade :
   Http.Request.t
  -> (conn -> 'b)
  -> [> `Expert of Http.Response.t * (Eio.Buf_read.t -> Eio.Buf_write.t -> 'b)
     | `Response of Cohttp_eio.Server.response Cohttp_eio.Server.IO.t
     ]
