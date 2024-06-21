class type t = Iostream_types.Seekable.t

let[@inline] seek self i : unit = self#seek i
let[@inline] pos self = self#pos ()
