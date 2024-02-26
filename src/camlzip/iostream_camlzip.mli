open Iostream

val decompress_in : In_buf.t -> In.t
val compress_in : ?level:int -> In_buf.t -> In.t
val compress_out : ?buf_size:int -> ?buf:bytes -> ?level:int -> Out.t -> Out.t
val decompress_out : ?buf_size:int -> ?buf:bytes -> Out.t -> Out.t
