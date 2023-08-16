(** Functions purely in userland *)

open Iostream

type filename = string

val with_in : filename -> (In.t -> 'a) -> 'a
val with_out : filename -> (Out.t -> 'a) -> 'a
val with_in_l : filename list -> (In.t list -> 'a) -> 'a

val concat : In.t list -> In.t
(** Read from each stream, in order *)

val tee : Out.t list -> Out.t
(** Write to all outputs simultaneously. *)

val read_all : In.t -> string
(** Input all the content into a string *)

val read_line : In_buf.t -> string option
val read_lines : In_buf.t -> string list
val read_lines' : In.t -> string list
val map_in_c : (char -> char) -> In.t -> In.t
val map_out_c : (char -> char) -> Out.t -> Out.t

val read_at_most : close_rec:bool -> int -> In_buf.t -> In.t
(** [read_at_most n ic] behaves like [ic] but stops after reading at
    most [n] bytes. Does not consume more than [n] bytes.
    @param close_rec if true, when closed, the inner channel [ic] is also closed
*)

val read_all_into_buf : In.t -> Buf.t -> unit
(** Input all the content into the buffer *)

val read_all_into_buffer : In.t -> Buffer.t -> unit
(** Input all the content into the buffer *)
