module Opcode = Opcode
module Mask = Mask
module Frame = Frame

module Private = struct
  module Utf8 = Utf8
  module Handshake = Handshake
end

exception Connection_closed = Conn.Connection_closed
exception Protocol_error = Conn.Protocol_error

module Message = Conn.Message

type conn = Conn.t

let recv = Conn.recv
let send = Conn.send
let close = Conn.close
let upgrade = Conn.upgrade
