class type t =
  object
    method seek : int -> unit
    method pos : unit -> int
  end

let[@inline] seek self i : unit = self#seek i
let[@inline] pos self = self#pos ()
