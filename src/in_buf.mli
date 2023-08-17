(** Buffered input stream. *)

type t = private {
  buf: bytes;
      (** The buffer. Do not modify directly, use {!fill_buffer}
            or {!fill_and_get} to refill it. *)
  mutable len: int;  (** Length of the slice in [buf]. *)
  mutable off: int;  (** Offset in [buf]. *)
  fill_buffer: bytes -> int;
      (** Refill [buf], return new [length]. The newly read bytes
          must live in [buf[0..len]]. *)
  close: unit -> unit;  (** Close the stream. Idempotent. *)
}

val create :
  ?buf:bytes -> ?close:(unit -> unit) -> fill_buffer:(bytes -> int) -> unit -> t
(** Create a new buffered input stream.
    @param fill_buffer will be called to refill the content of the buffer.
    @param buf the underlying buffer
    @raise Invalid_argument if the buffer's length is not at least 16. *)

val of_bytes : ?off:int -> ?len:int -> bytes -> t
(** Read from the given buffer.
    @param off initial offset (default 0)
    @param len length of the slice in the bytes. (default all available bytes from offset) *)

val of_unix_fd : ?buf:Bytes.t -> ?close_noerr:bool -> Unix.file_descr -> t
(** Create an in stream from a raw Unix file descriptor. The file descriptor
    must be opened for reading. *)

val of_in_channel : ?buf:bytes -> in_channel -> t
(** Wrap a standard input channel. *)

val open_file :
  ?buf:bytes -> ?mode:int -> ?flags:Unix.open_flag list -> string -> t

val with_open_file :
  ?buf:bytes ->
  ?mode:int ->
  ?flags:Unix.open_flag list ->
  string ->
  (t -> 'a) ->
  'a

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

val of_in : ?buf:bytes -> In.t -> t
(** Make a buffered version of the input stream.
    @param buf the buffer to use.
    @raise Invalid_argument if the buffer's length is not at least 16. *)

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

val input_all_into_buffer : t -> Buffer.t -> unit
(** Read the whole content into the given buffer. *)

val input_all : ?buffer:Buffer.t -> t -> string
(** Input all the content into a string *)

val copy_into : t -> Out.t -> unit
(** Copy the entire stream into the given output. *)

val input_line : ?buffer:Buffer.t -> t -> string option
(** Read a line from the input. Return [None] if the stream is empty.
   @param buffer a buffer to use to hold the line. *)

val input_lines : ?buffer:Buffer.t -> t -> string list
(** Read all lines from the input. *)
