(** Buffered output stream. *)

(** An output stream, ie. a place into which we can write bytes,
    with a buffer to amortize the cost of operations.

    This can be a [Buffer.t], an [out_channel], a [Unix.file_descr], etc. *)
class type t =
  object
    inherit Out.t

    method output_char : char -> unit
    (** Output a single char *)

    method flush : unit -> unit
    (** Flush underlying buffer *)
  end

class type t_seekable =
  object
    inherit t
    inherit Seekable.t
  end

val create :
  ?flush:(unit -> unit) ->
  ?close:(unit -> unit) ->
  output_char:(char -> unit) ->
  output:(bytes -> int -> int -> unit) ->
  unit ->
  t
(** Create a new output stream from raw components. *)

class dummy : t

val dummy : t
(** Dummy output, drops everything written to it. *)

(** Make a bufferized output from a non bufferized output+close *)
class virtual t_from_output :
  ?bytes:bytes
  -> unit
  -> object
       inherit t

       method virtual output_underlying : bytes -> int -> int -> unit
       (** Emit these bytes, unbufferized *)

       method virtual close_underlying : unit -> unit
       (** Close the underlying output. The bufferized output will
        flush and then call this. *)
     end

class bufferized : ?bytes:bytes -> Out.t -> t

val bufferized : ?bytes:bytes -> Out.t -> t

class of_out_channel : ?close_noerr:bool -> out_channel -> t_seekable

val of_out_channel : ?close_noerr:bool -> out_channel -> t_seekable
(** Wrap an out channel. *)

class of_buffer : Buffer.t -> t

val of_buffer : Buffer.t -> t
(** [of_buffer buf] is an output channel that writes directly into [buf].
    [flush] and [close] have no effect. *)

class open_file :
  ?close_noerr:bool
  -> ?mode:int
  -> ?flags:open_flag list
  -> string
  -> t_seekable

val open_file :
  ?close_noerr:bool ->
  ?mode:int ->
  ?flags:open_flag list ->
  string ->
  t_seekable
(** [open_file file] creates an out stream writing into the given file.
    @param mode permissions for the file creation
    @param flags set of unix flags to use. It must contain write permissions. *)

val with_open_file :
  ?close_noerr:bool ->
  ?mode:int ->
  ?flags:open_flag list ->
  string ->
  (t_seekable -> 'a) ->
  'a

val output_char : #t -> char -> unit
(** Output a single char *)

val output : #t -> bytes -> int -> int -> unit
(** Write the slice of bytes. *)

val close : #t -> unit
(** Close the stream. Idempotent. *)

val flush : #t -> unit
(** Ensure the bytes written so far are indeed written to the underlying
      storage/network socket/… and are not just sitting in a buffer. *)

val output_string : #t -> string -> unit
(** Output the whole string. *)

val output_line : #t -> string -> unit
(** Output the whole string followed by ['\n'].
    @since 0.2 *)

val output_lines : #t -> string Seq.t -> unit
(** Output a series of lines, each terminated by ['\n']. *)

val output_int : #t -> int -> unit
(** Output an integer in decimal notation. *)

val tee : t list -> t
(** [tee ocs] is an output that accepts bytes and writes them to every output
    in [ocs]. When closed, it closes all elements of [oc]. *)

val map_char : (char -> char) -> #t -> t
(** Transform the stream byte by byte *)
