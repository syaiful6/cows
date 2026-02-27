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

module Size_limit = struct
  type t =
    [ `No_size_limit
    | `Size_limit of int64
    ]

  let default_max_frame = `Size_limit (Int64.of_int (10 * 1024 * 1024))

  let at_most_limit : int64 -> t -> bool =
   fun x t -> match t with `No_size_limit -> true | `Size_limit l -> x <= l
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

let parse
      (type r)
      ?(size_limit = Size_limit.default_max_frame)
      (module R : READER with type t = r)
      (ic : r)
  =
  try
    let b0 = R.uint8 ic in
    let b1 = R.uint8 ic in
    let fin = b0 land 0x80 <> 0 in
    let rsv1 = b0 land 0x40 <> 0 in
    let rsv2 = b0 land 0x20 <> 0 in
    let rsv3 = b0 land 0x10 <> 0 in
    if rsv1 || rsv2 || rsv3 then raise (Parse_error Reserved_bits_set);
    let opcode = Opcode.of_int (b0 land 0x0f) in
    (match opcode with
    | `Ctrl i | `Non_ctrl i -> raise (Parse_error (Reserved_opcode i))
    | _ -> ());
    let masked = b1 land 0x80 <> 0 in
    let len7 = b1 land 0x7f in
    let payload_len =
      match len7 with
      | 126 ->
        let n = R.uint16 ic in
        if not (Size_limit.at_most_limit (Int64.of_int n) size_limit)
        then raise (Parse_error Frame_too_large);
        n
      | 127 ->
        let n = R.uint64 ic in
        if not (Size_limit.at_most_limit n size_limit)
        then raise (Parse_error Frame_too_large);
        Int64.to_int n
      | n ->
        if not (Size_limit.at_most_limit (Int64.of_int n) size_limit)
        then raise (Parse_error Frame_too_large);
        n
    in
    (match Opcode.to_kind opcode with
    | `Control ->
      if not fin then raise (Parse_error Control_frame_fragmented);
      if payload_len > 125 then raise (Parse_error Control_frame_too_large)
    | `Non_control -> ());
    let key =
      if masked
      then
        let s = R.take 4 ic in
        Some
          ((Char.code s.[0] lsl 24)
          lor (Char.code s.[1] lsl 16)
          lor (Char.code s.[2] lsl 8)
          lor Char.code s.[3])
      else None
    in
    let raw = R.take payload_len ic in
    let content = match key with None -> raw | Some k -> Mask.apply k raw in
    Ok { fin; rsv1; rsv2; rsv3; opcode; content }
  with
  | Parse_error e -> Error e
  | End_of_file -> Error Insufficient_data

let serialize (type w) ?key (module W : WRITER with type t = w) (oc : w) frame =
  let payload =
    match key with
    | None -> frame.content
    | Some k -> Mask.apply k frame.content
  in
  let len = String.length payload in
  let b0 =
    (if frame.fin then 0x80 else 0)
    lor (if frame.rsv1 then 0x40 else 0)
    lor (if frame.rsv2 then 0x20 else 0)
    lor (if frame.rsv3 then 0x10 else 0)
    lor Opcode.to_int frame.opcode
  in
  W.uint8 oc b0;
  let masked_bit = match key with Some _ -> 0x80 | None -> 0 in
  if len < 126
  then W.uint8 oc (masked_bit lor len)
  else if len < 65536
  then begin
    W.uint8 oc (masked_bit lor 126);
    W.uint16 oc len
  end
  else begin
    W.uint8 oc (masked_bit lor 127);
    W.uint64 oc (Int64.of_int len)
  end;
  (match key with
  | None -> ()
  | Some k ->
    let buf = Bytes.create 4 in
    Bytes.set buf 0 (Char.chr ((k lsr 24) land 0xff));
    Bytes.set buf 1 (Char.chr ((k lsr 16) land 0xff));
    Bytes.set buf 2 (Char.chr ((k lsr 8) land 0xff));
    Bytes.set buf 3 (Char.chr (k land 0xff));
    W.string oc (Bytes.unsafe_to_string buf));
  W.string oc payload
