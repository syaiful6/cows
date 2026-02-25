(* WebSocket payload masking per RFC 6455 §5.3.
   Masking is symmetric: applying the same key twice recovers the original.
   The key is a 32-bit value; byte j of the key is (key lsr ((3 - j) * 8)) land 0xff. *)

let apply key data =
  let len = String.length data in
  let buf = Bytes.of_string data in
  for i = 0 to len - 1 do
    let j = i land 3 in
    let key_byte = (key lsr ((3 - j) * 8)) land 0xff in
    let byte = Char.code (Bytes.get buf i) lxor key_byte in
    Bytes.set buf i (Char.chr byte)
  done;
  Bytes.unsafe_to_string buf
