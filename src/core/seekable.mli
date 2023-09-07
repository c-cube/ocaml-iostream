(** An object we can seek in.

    Files can be seeked in, i.e the read/write head can move
    around. *)

class type t =
  object
    method seek : int -> unit
    (** Seek in the underlying stream.
        @raise Sys_error in case of failure *)

    method pos : unit -> int
    (** Return current offset in underlying stream.
        @raise Sys_error in case of failure *)
  end

val seek : #t -> int -> unit
val pos : #t -> int
