module Slice = struct
  type t = {
    bytes: bytes;  (** Bytes *)
    mutable off: int;  (** Offset in bytes *)
    mutable len: int;  (** Length of the slice. Empty slice has [len=0] *)
  }
end

module Seekable = struct
  class type t = object
    method seek : int -> unit
    method pos : unit -> int
  end
end

module In = struct
  class type t = object
    method input : bytes -> int -> int -> int
    (** Read into the slice. Returns [0] only if the
        stream is closed. *)

    method close : unit -> unit
    (** Close the input. Must be idempotent. *)
  end

  class type t_seekable = object
    inherit t
    inherit Seekable.t
  end
end

module In_buf = struct
  class type t = object
    inherit In.t
    method fill_buf : unit -> Slice.t
    method consume : int -> unit
  end
end

module Out = struct
  class type t = object
    method output : bytes -> int -> int -> unit
    method close : unit -> unit
  end

  class type t_seekable = object
    inherit t
    inherit Seekable.t
  end
end

module Out_buf = struct
  class type t = object
    method output_char : char -> unit
    method output : bytes -> int -> int -> unit
    method flush : unit -> unit
    method close : unit -> unit
  end

  class type t_seekable = object
    inherit t
    inherit Seekable.t
  end
end
