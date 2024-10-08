(** Buffered input stream. *)

(** The implementation of buffered input streams. *)
class type t = object
  inherit In.t

  method fill_buf : unit -> Slice.t
  (** [ic#fill_buf()] returns a slice into the [ic]'s internal buffer,
        and ensures it's empty only if [ic.ic] is empty. In other
        words, the invariant is that this only returns
        an empty slice if the input stream is exhausted. *)

  method consume : int -> unit
  (** Consume [n] bytes from the inner buffer. This is only
        valid if the last call to [fill_buf] returned a slice with
        at least [n] bytes. *)
end

(** A mixin to implement a buffered input by only providing
    a [refill] method. Add a [close] method and it's good to go. *)
class virtual t_from_refill : ?bytes:bytes -> unit -> object
  method virtual private refill : Slice.t -> unit
  (** Implementation of the stream: this takes a slice,
        resets its offset, and fills it with bytes. It must write
        at least one byte in the slice, unless the underlying
        input has reached its end. *)

  method input : bytes -> int -> int -> int
  method fill_buf : unit -> Slice.t
  method consume : int -> unit
end

val create :
  ?bytes:bytes -> ?close:(unit -> unit) -> refill:(bytes -> int) -> unit -> t
(** Create a new buffered input stream.
    @param refill will be called to refill the content of the bytes,
    returning how many bytes were added (starting at offset 0).
    @param buf the underlying buffer
    @raise Invalid_argument if the buffer's length is not at least 16. *)

class of_bytes : ?off:int -> ?len:int -> bytes -> t

val of_bytes : ?off:int -> ?len:int -> bytes -> t

class of_string : ?off:int -> ?len:int -> string -> t

val of_string : ?off:int -> ?len:int -> string -> t

class bufferized : ?bytes:bytes -> In.t -> t

val bufferized : ?bytes:bytes -> In.t -> t

(* val of_bytes : ?off:int -> ?len:int -> bytes -> t *)
(** Read from the given buffer.
    @param off initial offset (default 0)
    @param len length of the slice in the bytes. (default all available bytes from offset) *)

class of_in_channel : ?bytes:bytes -> in_channel -> t

val of_in_channel : ?bytes:bytes -> in_channel -> t
(** Wrap a standard input channel. *)

class open_file :
  ?bytes:bytes ->
  ?mode:int ->
  ?flags:open_flag list ->
  string ->
  t

val open_file :
  ?bytes:bytes -> ?mode:int -> ?flags:open_flag list -> string -> t

val with_open_file :
  ?bytes:bytes ->
  ?mode:int ->
  ?flags:open_flag list ->
  string ->
  (t -> 'a) ->
  'a

val fill_buf : #t -> Slice.t
(** [fill_buffer bic] returns a slice into [bic]'s internal buffer,
      and ensures it's empty only if [bic.ic]
      is empty. *)

val input : #t -> bytes -> int -> int -> int
(** Read into the given slice of bytes. *)

val of_in : ?bytes:bytes -> #In.t -> t
(** Make a buffered version of the input stream.
    @param bytes the buffer to use.
    @raise Invalid_argument if the buffer's length is not at least 16. *)

val consume : #t -> int -> unit
(** [consume bic n] consumes [n] bytes from [bic].
      Precondition: [n <= get_len bic], ie. one cannot consume bytes that have
      not yet been obtained via {!fill_buffer} or {!fill_and_get}. *)

val close : #t -> unit
(** Close the input stream. *)

val into_in : #t -> In.t
(** Cast into a {!In.t}. This doesn't allocate. *)

val input_all_into_buffer : #t -> Buffer.t -> unit
(** Read the whole content into the given buffer. *)

val input_all : ?buf:bytes -> #t -> string
(** [input_all ic] reads the whole content of [ic] into a string.
    @param buf the initial buffer to use internally.
    @since 0.2 *)

val copy_into : #t -> #Out.t -> unit
(** Copy the entire stream into the given output. *)

val skip : #t -> int -> unit
(** [skip ic n] reads and dicards the next [n] bytes in [ic]. *)

val input_line : ?buffer:Buffer.t -> #t -> string option
(** Read a line from the input. Return [None] if the stream is empty.
   @param buffer a buffer to use to hold the line. *)

val input_lines : ?buffer:Buffer.t -> #t -> string list
(** Read all lines from the input. *)

val to_iter : #t -> (char -> unit) -> unit
val to_seq : #t -> char Seq.t
val of_seq : ?bytes:bytes -> char Seq.t -> t
