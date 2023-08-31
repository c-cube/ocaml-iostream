class virtual t =
  object
    method virtual seek : int -> unit
    method virtual pos : unit -> int
  end

let[@inline] seek self i : unit = self#seek i
let[@inline] pos self = self#pos ()
