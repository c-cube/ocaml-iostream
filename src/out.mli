(** Output stream. *)

class virtual cls :
  object
    method virtual output_char : char -> unit
    (** Output a single char *)

    method virtual output : bytes -> int -> int -> unit
    (** Output slice *)

    method flush : unit -> unit
    (** Flush underlying buffer *)

    method close : unit -> unit
    (** Close the output. Must be idempotent. *)

    method as_fd : unit -> Unix.file_descr option
  end

type t = cls
(** An output stream, ie. a place into which we can write bytes.

    This can be a [Buffer.t], an [out_channel], a [Unix.file_descr], etc. *)

val create :
  ?as_fd:(unit -> Unix.file_descr option) ->
  ?flush:(unit -> unit) ->
  ?close:(unit -> unit) ->
  output_char:(char -> unit) ->
  output:(bytes -> int -> int -> unit) ->
  unit ->
  t
(** Create a new output stream from raw components. *)

val dummy : t
(** Dummy output, drops everything written to it. *)

val of_out_channel : ?close_noerr:bool -> out_channel -> t
(** Wrap an out channel. *)

val of_unix_fd : Unix.file_descr -> t
(** Output stream directly writing into the given Unix file descriptor. *)

val of_buffer : Buffer.t -> t
(** [of_buffer buf] is an output channel that writes directly into [buf].
    [flush] and [close] have no effect. *)

val open_file : ?mode:int -> ?flags:Unix.open_flag list -> string -> t
(** [open_file file] creates an out stream writing into the given file.
    @param mode permissions for the file creation
    @param flags set of unix flags to use. It must contain write permissions. *)

val with_open_file :
  ?mode:int -> ?flags:Unix.open_flag list -> string -> (t -> 'a) -> 'a

val output_char : t -> char -> unit
(** Output a single char *)

val output : t -> bytes -> int -> int -> unit
(** Write the slice of bytes. *)

val close : t -> unit
(** Close the stream. Idempotent. *)

val flush : t -> unit
(** Ensure the bytes written so far are indeed written to the underlying
      storage/network socket/… and are not just sitting in a buffer. *)

val output_string : t -> string -> unit
(** Output the whole string. *)

val output_lines : t -> string Seq.t -> unit
(** Output a series of lines, each terminated by ['\n']. *)

val output_int : t -> int -> unit
(** Output an integer in decimal notation. *)

val tee : t list -> t
(** [tee ocs] is an output that accepts bytes and writes them to every output
    in [ocs]. When closed, it closes all elements of [oc]. *)

val pos : t -> int
(** Current position in the underlying file, if any.
      @raise Sys_error if this is not a wrapper around a unix FD. *)

val seek : t -> int -> unit
(** Move to the given location.
      @raise Sys_error if this is not a wrapper around a unix FD. *)

val map_char : (char -> char) -> t -> t
(** Transform the stream byte by byte *)
