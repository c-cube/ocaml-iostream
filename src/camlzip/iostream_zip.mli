
open IO

val decode : ?buf_size:int -> In.t -> In.t

val encode : ?buf_size:int -> Out.t -> Out.t

val encode_in : ?buf_size:int -> In.t -> In.t
