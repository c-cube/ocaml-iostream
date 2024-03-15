open Iostream

val decompress_in : #In_buf.t -> In.t
(** [decompress_in ic] returns a new input stream
    that is the decompressed version of [ic] *)

val decompress_in_buf : ?buf_size:int -> ?buf:bytes -> #In_buf.t -> In_buf.t
(** Like {!decompress_in} but the output is buffered as well. *)

val compress_in : ?level:int -> #In_buf.t -> In.t
(** [compress_in ?level ic] is a new input stream
    that is the compressed version of [ic].
    @param level optional Zlib compression level *)

val compress_in_buf :
  ?buf_size:int -> ?buf:bytes -> ?level:int -> #In_buf.t -> In_buf.t
(** Same as {!compress_in} but returning a buffered input. *)

val compressed_out :
  ?buf_size:int -> ?buf:bytes -> ?level:int -> #Out.t -> Out_buf.t
(** [compressed_out oc] takes a output stream [oc], and
    returns a new output stream [oc2]. Writing some (normal) bytes to [oc2]
    will write a compressed version of these bytes into [oc] (possibly
    after a flush). *)

val compressed_out_buf :
  ?buf_size:int -> ?buf:bytes -> ?level:int -> #Out_buf.t -> Out_buf.t

val decompressed_out : ?buf_size:int -> ?buf:bytes -> #Out.t -> Out_buf.t
(** [decompressed_out oc] is a new output stream [oc2]. Writing
    (compressed) bytes to [oc2] will write their decompressed version
    into [oc] (possibly after a flush) *)

val decompressed_out_buf :
  ?buf_size:int -> ?buf:bytes -> #Out_buf.t -> Out_buf.t
