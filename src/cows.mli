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

  module Size_limit : sig
    type t =
      [ `No_size_limit
      | `Size_limit of int64
      ]
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

  val parse :
     ?size_limit:Size_limit.t
    -> (module READER with type t = 'r)
    -> 'r
    -> (t, error) result

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

exception Connection_closed
exception Protocol_error of string

module Message : sig
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

type conn

val recv : conn -> Message.t
val send : conn -> Message.t -> unit

val close : ?code:int -> ?reason:string -> conn -> unit
(** [close ?code ?reason conn] sends a Close frame to the peer.

    When initiating the close, continue calling [recv] to drain any in-flight
    messages. Once [recv] returns [`Close] (the peer's echo), the handshake is
    complete and you should stop calling [recv].

    When the peer initiates the close ([recv] returns [`Close] first), call
    [close] to send the echo and then stop calling [recv].

    In either case, [recv] may raise [Connection_closed] if the peer drops
    the connection without completing the close handshake. *)

val upgrade :
   ?size_limit:Frame.Size_limit.t
  -> Http.Request.t
  -> (conn -> unit)
  -> Cohttp_eio.Server.response_action
(** [upgrade ?size_limit req handler] performs the WebSocket handshake on [req]
    and runs [handler] with the resulting connection. [size_limit] caps the payload
    length accepted for every frame. Defaults to [`Size_limit 64MB]. Frames
    exceeding the limit are rejected with a [Protocol_error]. *)
