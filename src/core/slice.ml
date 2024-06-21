(** Byte slice or buffer. *)

type t = Iostream_types.Slice.t = {
  bytes: bytes;  (** Bytes *)
  mutable off: int;  (** Offset in bytes *)
  mutable len: int;  (** Length of the slice. Empty slice has [len=0] *)
}
(** A slice of bytes.
    The valid bytes in the slice are [bytes[off], bytes[off+1], …, bytes[off+len-1]]
    (i.e [len] bytes starting at offset [off]). *)

let empty : t = { bytes = Bytes.create 0; off = 0; len = 0 }

let create size : t =
  let size = max 16 size in
  if size > Sys.max_string_length then
    invalid_arg "Slice.create: size is too big";
  { bytes = Bytes.create size; off = 0; len = 0 }

let[@inline] of_bytes bs : t = { bytes = bs; off = 0; len = 0 }
let[@inline] bytes self = self.bytes
let[@inline] off self = self.off
let[@inline] len self = self.len

(** Consume the first [n] bytes from the slice, making it [n] bytes
    shorter. This modifies the slice in place. *)
let[@inline] consume (self : t) n : unit =
  if n < 0 || n > self.len then invalid_arg "In_buf.consume_buf";
  self.off <- self.off + n;
  self.len <- self.len - n

(** find index of [c] in slice, or raise [Not_found] *)
let find_index_exn (self : t) c : int =
  let found = ref false in
  let i = ref self.off in
  let limit = self.off + self.len in
  while (not !found) && !i < limit do
    let c' = Bytes.unsafe_get self.bytes !i in
    if c = c' then
      found := true
    else
      incr i
  done;
  if !found then
    !i
  else
    raise Not_found
