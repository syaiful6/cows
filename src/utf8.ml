(* UTF-8 validation per RFC 3629.
   Returns false on: overlong encodings, surrogates (U+D800-U+DFFF),
   codepoints above U+10FFFF, truncated sequences, and invalid lead bytes. *)

let is_valid s =
  let n = String.length s in
  let i = ref 0 in
  try
    while !i < n do
      let b0 = Char.code (String.unsafe_get s !i) in
      if b0 lsr 7 = 0
      then (* 0xxxxxxx: U+0000..U+007F *)
        incr i
      else if b0 lsr 5 = 0b110
      then begin
        (* 110xxxxx 10xxxxxx: U+0080..U+07FF *)
        if b0 < 0xC2 then raise Exit;
        (* overlong: would encode < U+0080 *)
        if !i + 1 >= n then raise Exit;
        let b1 = Char.code (String.unsafe_get s (!i + 1)) in
        if b1 lsr 6 <> 0b10 then raise Exit;
        i := !i + 2
      end else if b0 lsr 4 = 0b1110
      then begin
        (* 1110xxxx 10xxxxxx 10xxxxxx: U+0800..U+FFFF *)
        if !i + 2 >= n then raise Exit;
        let b1 = Char.code (String.unsafe_get s (!i + 1)) in
        let b2 = Char.code (String.unsafe_get s (!i + 2)) in
        if b1 lsr 6 <> 0b10 || b2 lsr 6 <> 0b10 then raise Exit;
        if b0 = 0xE0 && b1 < 0xA0 then raise Exit;
        (* overlong: < U+0800 *)
        if b0 = 0xED && b1 >= 0xA0 then raise Exit;
        (* surrogate: U+D800..U+DFFF *)
        i := !i + 3
      end else if b0 lsr 3 = 0b11110
      then begin
        (* 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx: U+10000..U+10FFFF *)
        if b0 > 0xF4 then raise Exit;
        (* above U+10FFFF *)
        if !i + 3 >= n then raise Exit;
        let b1 = Char.code (String.unsafe_get s (!i + 1)) in
        let b2 = Char.code (String.unsafe_get s (!i + 2)) in
        let b3 = Char.code (String.unsafe_get s (!i + 3)) in
        if b1 lsr 6 <> 0b10 || b2 lsr 6 <> 0b10 || b3 lsr 6 <> 0b10
        then raise Exit;
        if b0 = 0xF0 && b1 < 0x90 then raise Exit;
        (* overlong: < U+10000 *)
        if b0 = 0xF4 && b1 > 0x8F then raise Exit;
        (* above U+10FFFF *)
        i := !i + 4
      end else raise Exit
      (* 10xxxxxx continuation as lead, or 0xF8-0xFF *)
    done;
    true
  with Exit -> false
