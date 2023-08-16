(** Input stream. *)

type t = private {
  input: bytes -> int -> int -> int;
      (** Read into the slice. Returns [0] only if the
        stream is closed. *)
  close: unit -> unit;  (** Close the input. Must be idempotent. *)
  as_fd: unit -> Unix.file_descr option;
      (** Cast into a file descriptor, {b if} it actually is a direct wrapper of a
        Unix FD. *)
}
(** An input stream, i.e an incoming stream of bytes.

      This can be a [string], an [int_channel], an [Unix.file_descr], a
      decompression wrapper around another input stream, etc. *)

val create :
  ?as_fd:(unit -> Unix.file_descr option) ->
  ?close:(unit -> unit) ->
  input:(bytes -> int -> int -> int) ->
  unit ->
  t

val empty : t
(** Empty input, contains 0 bytes. *)

val of_unix_fd : ?close_noerr:bool -> Unix.file_descr -> t
(** Create an in stream from a raw Unix file descriptor. The file descriptor
      must be opened for reading. *)

val of_in_channel : ?close_noerr:bool -> in_channel -> t
(** Wrap a standard input channel. *)

val of_string : ?off:int -> ?len:int -> string -> t
(** An input channel reading from the string.
    @param offset initial offset in the string. Default [0].
    @param len the length of the slice we read from. Default [String.length s - off].
*)

val of_bytes : ?off:int -> ?len:int -> bytes -> t
(** An input channel reading from the bytes buffer. See {!of_string}
    for more details. *)

val input : t -> bytes -> int -> int -> int
(** Read bytes into the given buffer. This returns [0] only if
    the stream has reached its end. *)

val input_into_buf : t -> Buf.t -> unit
(** [input_into_buf is buf] clears [buf] and tries to read as many
    bytes as possible into [buf] as it can in one go.
    If it reads [n] bytes, it sets [buf.len] to [n]. This only reads 0
    bytes if the stream has reached its end *)

val seek : t -> int64 -> unit
(** If available, seek in the underlying stream.
      @raise Sys_error in case of failure *)

val pos : t -> int64
(** If available, return current offset in underlying stream.
      @raise Sys_error in case of failure *)

val close : t -> unit
(** Close the input stream. This is idempotent. *)

val copy_into : ?buf:Buf.t -> t -> Out.t -> unit
(** Copy the whole stream into the given output. *)
