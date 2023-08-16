(** Buffered input stream. *)

type t = private {
  mutable buf: Buf.t;
      (** The buffer. Do not modify directly, use {!fill_buffer}
            or {!fill_and_get} to refill it. *)
  mutable off: int;
  fill_buffer: t -> int;
      (** Refill [buf], return new offset, modifies size of [buf].
            Precondition: [Buf.size buf=0] *)
  close: unit -> unit;  (** Close the stream. Idempotent. *)
}

val create :
  ?buf:Buf.t -> ?close:(unit -> unit) -> fill_buffer:(t -> int) -> unit -> t
(** Create a new buffered input stream.
     @param fill_buffer will be called *)

val fill_buffer : t -> unit
(** [fill_buffer bic] ensures that [bic.buf] is empty only if [bic.ic]
      is empty. Always call this before looking at [bic.buf].
  *)

val fill_and_get : t -> bytes * int * int
(** Ensure the underlying buffer is not empty (unless end-of-input was reached),
      and return the active slice of it as a tuple [bytes, offset, len].

      Postconditions: [len = 0] if and only if the whole input stream has been consumed.
    *)

val get_bytes : t -> bytes
(** Direct access to the raw bytes of the underlying buffer. *)

val get_off : t -> int
(** Current offset in the underlying buffer. *)

val get_len : t -> int
(** Current length of the underlying buffer. *)

val input : t -> bytes -> int -> int -> int
(** Read into the given slice of bytes. *)

val of_in : ?buf:Buf.t -> In.t -> t
(** Make a buffered version of the input stream.
      @param buf the buffer to use. It will be cleared. *)

val consume : t -> int -> unit
(** [consume bic n] consumes [n] bytes from [bic].
      Precondition: [n <= get_len bic], ie. one cannot consume bytes that have
      not yet been obtained via {!fill_buffer} or {!fill_and_get}. *)

val close : t -> unit
(** Close the input stream. *)

val into_in : t -> In.t
(** Cast into a {!In.t}. Note that this does allocate a new record, so it's
      advised to not perform this operation in a tight loop.
      The function {!input} can be called directly on the buffered stream if needed. *)

val copy_into : t -> Out.t -> unit
(** Copy the entire stream into the given output. *)
