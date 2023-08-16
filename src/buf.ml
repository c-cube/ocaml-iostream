(** Small internal buffer.

    This implementation of buffers exposes its internal slice of bytes. This
    is convenient for parsers/lexers that need direct access to the buffer
    of a buffered input stream (e.g. to read a line, read a HTTP header, etc.) *)

type t = {
  mutable bytes: bytes;
      (** The buffer itself. Only a slice of it contains meaningful data,
    as delimited by the [len] field. *)
  mutable len: int;
      (** Length of the valid slice of [bytes].
    The valid slice is [bytes[0..len]] (with index [len] excluded). *)
}
(** Small buffer implementation. *)

(** Create a new buffer.
    @param size the size of the internal slice of bytes. *)
let create ?(size = 4_096) () : t = { bytes = Bytes.create size; len = 0 }

(** Length of the valid slice of the buffer. *)
let[@inline] size self = self.len

(** Direct access to the bytes. *)
let[@inline] bytes_slice self = self.bytes

(** Set length to 0, meaning the buffer is now empty. *)
let[@inline] clear self : unit = self.len <- 0

let[@inline] is_empty self = self.len = 0

(** Ensure the buffer can contain at least [new_size] bytes, possibly resizing
    the internal bytes slice. *)
let ensure_capacity self new_desired_cap : unit =
  let grow_ new_cap =
    let new_buf = Bytes.create new_cap in
    Bytes.blit self.bytes 0 new_buf 0 self.len;
    self.bytes <- new_buf
  in

  (* need to resize? *)
  if new_desired_cap >= Bytes.length self.bytes then (
    (* ensure amortization over successive calls to [ensure_capacity] *)
    let new_actual_cap =
      min Sys.max_string_length
        (max new_desired_cap (self.len + (self.len / 2) + 10))
    in
    if new_actual_cap < new_desired_cap then
      failwith "Buf: maximum bytes size exceeded.";

    grow_ new_actual_cap
  )

(** Push a single char. *)
let add_char (self : t) c : unit =
  ensure_capacity self (self.len + 1);
  Bytes.set self.bytes self.len c;
  self.len <- self.len + 1

(** Append some bytes to the buffer. *)
let add_bytes (self : t) b i len : unit =
  ensure_capacity self (self.len + len);
  (* resize if needed? *)
  Bytes.blit b i self.bytes self.len len;
  self.len <- self.len + len

(** Get a copy of the contents as a string. *)
let contents (self : t) : string = Bytes.sub_string self.bytes 0 self.len
