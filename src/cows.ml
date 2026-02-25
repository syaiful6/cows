module Opcode = Opcode
module Mask = Mask
module Frame = Frame

module Private = struct
  module Utf8 = Utf8
  module Handshake = Handshake
end

type close_reason = Conn.close_reason =
  | Clean of int * string
  | Abnormal

exception Connection_closed = Conn.Connection_closed
exception Protocol_error = Conn.Protocol_error

type message = Conn.message =
  | Text of string
  | Binary of string
  | Ping of string
  | Pong of string

type conn = Conn.t

let recv = Conn.recv
let send = Conn.send
let close = Conn.close
let upgrade = Conn.upgrade
