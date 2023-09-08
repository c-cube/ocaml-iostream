(** Input stream. *)

(** An input stream, i.e an incoming stream of bytes.

    This can be a [string], an [int_channel], an [Unix.file_descr], a
    decompression wrapper around another input stream, etc. *)
class type t =
  object
    method input : bytes -> int -> int -> int
    (** Read into the slice. Returns [0] only if the
        stream is closed. *)

    method close : unit -> unit
    (** Close the input. Must be idempotent. *)
  end

(** An input stream that is also seekable. *)
class type t_seekable =
  object
    inherit t
    inherit Seekable.t
  end

val create :
  ?close:(unit -> unit) -> input:(bytes -> int -> int -> int) -> unit -> t

val empty : t
(** Empty input, contains 0 bytes. *)

val of_in_channel : ?close_noerr:bool -> in_channel -> t_seekable
(** Wrap a standard input channel. *)

val open_file :
  ?close_noerr:bool ->
  ?mode:int ->
  ?flags:open_flag list ->
  string ->
  t_seekable

val with_open_file :
  ?close_noerr:bool ->
  ?mode:int ->
  ?flags:open_flag list ->
  string ->
  (t_seekable -> 'a) ->
  'a

val of_string : ?off:int -> ?len:int -> string -> t_seekable
(** An input channel reading from the string.
    @param offset initial offset in the string. Default [0].
    @param len the length of the slice we read from. Default [String.length s - off].
*)

val of_bytes : ?off:int -> ?len:int -> bytes -> t_seekable
(** An input channel reading from the bytes buffer. See {!of_string}
    for more details. *)

val input : #t -> bytes -> int -> int -> int
(** Read bytes into the given buffer. This returns [0] only if
    the stream has reached its end.
    @raise Invalid_argument if the arguments do not denote a valid slice.
*)

val input_all : ?buf:bytes -> #t -> string
(** [input_all ic] reads the whole content of [ic] into a string.
    @param buf the initial buffer to use internally.
    @since 0.2 *)

val really_input : #t -> bytes -> int -> int -> unit
(** Same as [input], but reads exactly the demanded amount of bytes.
    @raise Invalid_argument if the arguments do not denote a valid slice.
    @raise End_of_file if the input does not contain enough data.
    @since 0.2
*)

val really_input_string : #t -> int -> string
(** [really_input_string ic n] reads exactly [n] bytes of [ic]
    and returns them as a string.
    @raise End_of_file if the input does not contain enough data.
    @since 0.2
*)

val concat : t list -> t
(** Read from each stream, in order *)

val close : #t -> unit
(** Close the input stream. This is idempotent. *)

val copy_into : ?buf:bytes -> #t -> Out.t -> unit
(** Copy the whole stream into the given output. *)

val map_char : (char -> char) -> #t -> t
(** Transform the stream byte by byte *)
