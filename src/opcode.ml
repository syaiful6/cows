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

let of_int i =
  match i land 0xf with
  | 0x0 -> `Continuation
  | 0x1 -> `Text
  | 0x2 -> `Binary
  | 0x8 -> `Close
  | 0x9 -> `Ping
  | 0xA -> `Pong
  | i when i > 2 && i < 8 -> `Non_ctrl i
  | i -> `Ctrl i

let to_int = function
  | `Continuation -> 0x0
  | `Text -> 0x1
  | `Binary -> 0x2
  | `Close -> 0x8
  | `Ping -> 0x9
  | `Pong -> 0xA
  | `Ctrl i | `Non_ctrl i -> i

type kind =
  [ `Control
  | `Non_control
  ]

let to_kind = function
  | `Continuation | `Text | `Binary | `Non_ctrl _ -> `Non_control
  | `Close | `Ping | `Pong | `Ctrl _ -> `Control
