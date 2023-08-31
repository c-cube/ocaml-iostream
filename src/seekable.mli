(** An object we can seek in.

    Files can be seeked in, i.e the read/write head can move
    around. *)

class virtual t :
  object
    method virtual seek : int -> unit
    (** If available, seek in the underlying stream.
        @raise Sys_error in case of failure *)

    method virtual pos : unit -> int
    (** If available, return current offset in underlying stream.
        @raise Sys_error in case of failure *)
  end

val seek : #t -> int -> unit
val pos : #t -> int
