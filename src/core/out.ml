class type t =
  object
    method output : bytes -> int -> int -> unit
    method close : unit -> unit
  end

class type t_seekable =
  object
    inherit t
    inherit Seekable.t
  end

class dummy : t =
  object
    method close () = ()
    method output _ _ _ = ()
  end

let dummy : t = new dummy

(** [of_out_channel oc] wraps the channel into a {!Out_channel.t}.
      @param close_noerr if true, then closing the result uses [close_out_noerr]
      instead of [close_out] to close [oc] *)
class of_out_channel ?(close_noerr = false) (oc : out_channel) : t_seekable =
  object
    method output bs i len = output oc bs i len

    method close () =
      if close_noerr then
        close_out_noerr oc
      else (
        flush oc;
        close_out oc
      )

    method seek i = seek_out oc i
    method pos () = pos_out oc
  end

let[@inline] of_out_channel ?close_noerr oc = new of_out_channel ?close_noerr oc

let open_file ?close_noerr ?(mode = 0o644)
    ?(flags = [ Open_binary; Open_wronly; Open_creat; Open_trunc ]) filename :
    t_seekable =
  let oc = open_out_gen flags mode filename in
  of_out_channel ?close_noerr oc

let with_open_file ?close_noerr ?mode ?flags filename f =
  let oc = open_file ?close_noerr ?mode ?flags filename in
  Fun.protect ~finally:oc#close (fun () -> f oc)

class of_buffer (buf : Buffer.t) : t =
  object
    method close () = ()
    method output bs i len = Buffer.add_subbytes buf bs i len
  end

let[@inline] of_buffer buf = new of_buffer buf

(** Output the buffer slice into this channel *)
let[@inline] output (self : #t) buf i len : unit = self#output buf i len

let[@inline] output_string (self : #t) (str : string) : unit =
  self#output (Bytes.unsafe_of_string str) 0 (String.length str)

(** Close the channel. *)
let[@inline] close self : unit = self#close ()

let output_int self i =
  let s = string_of_int i in
  output_string self s

let tee (l : t list) : t =
  match l with
  | [] -> dummy
  | [ oc ] -> oc
  | _ ->
    object
      method output bs i len = List.iter (fun oc -> output oc bs i len) l
      method close () = List.iter close l
    end

class map_char f (oc : #t) : t =
  object
    method output buf i len =
      for j = i to i + len - 1 do
        let c = Bytes.get buf j in
        (* safety: [j] is valid because [get] above did not raise *)
        Bytes.unsafe_set buf j (f c)
      done;
      output oc buf i len

    method close () = close oc
  end

let[@inline] map_char f oc = new map_char f oc
