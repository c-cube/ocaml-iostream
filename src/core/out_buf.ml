class type t =
  object
    method output_char : char -> unit
    method output : bytes -> int -> int -> unit
    method flush : unit -> unit
    method close : unit -> unit
  end

class type t_seekable =
  object
    inherit t
    inherit Seekable.t
  end

let create ?(flush = ignore) ?(close = ignore) ~output_char ~output () : t =
  object
    method flush () = flush ()
    method close () = close ()
    method output_char c = output_char c
    method output bs i len = output bs i len
  end

class dummy : t =
  object
    inherit Out.dummy
    method flush () = ()
    method output_char _ = ()
  end

let dummy = new dummy

(** [of_out_channel oc] wraps the channel into a {!Out_channel.t}.
      @param close_noerr if true, then closing the result uses [close_out_noerr]
      instead of [close_out] to close [oc] *)
class of_out_channel ?close_noerr (oc : out_channel) : t_seekable =
  object
    inherit Out.of_out_channel ?close_noerr oc
    method output_char c = output_char oc c
    method flush () = flush oc
  end

let[@inline] of_out_channel ?close_noerr oc = new of_out_channel ?close_noerr oc

class open_file ?close_noerr ?(mode = 0o644)
  ?(flags = [ Open_binary; Open_wronly; Open_creat; Open_trunc ]) filename :
  t_seekable =
  let oc = open_out_gen flags mode filename in
  of_out_channel ?close_noerr oc

let[@inline] open_file ?close_noerr ?mode ?flags filename =
  new open_file ?close_noerr ?mode ?flags filename

let with_open_file ?close_noerr ?mode ?flags filename f =
  let oc = open_file ?close_noerr ?mode ?flags filename in
  Fun.protect ~finally:oc#close (fun () -> f oc)

class of_buffer (buf : Buffer.t) : t =
  object
    inherit Out.of_buffer buf
    method flush () = ()
    method output_char c = Buffer.add_char buf c
  end

let[@inline] of_buffer buf = new of_buffer buf

(** Output the buffer slice into this channel *)
let[@inline] output_char (self : #t) c : unit = self#output_char c

let output = Out.output
let output_string = Out.output_string

let output_line (self : #t) (str : string) : unit =
  output_string self str;
  output_char self '\n'

let close = Out.close
let output_int = Out.output_int

(** Flush (ie. force write) any buffered bytes. *)
let[@inline] flush self : unit = self#flush ()

let output_lines self seq = Seq.iter (output_line self) seq

let tee (l : t list) : t =
  match l with
  | [] -> dummy
  | [ oc ] -> oc
  | _ ->
    object
      method output bs i len = List.iter (fun oc -> output oc bs i len) l
      method output_char c = List.iter (fun oc -> output_char oc c) l
      method close () = List.iter close l
      method flush () = List.iter flush l
    end

class map_char f (oc : #t) : t =
  object
    method output_char c = output_char oc (f c)

    method output buf i len =
      for j = i to i + len - 1 do
        let c = Bytes.get buf j in
        (* safety: [j] is valid because [get] above did not raise *)
        Bytes.unsafe_set buf j (f c)
      done;
      output oc buf i len

    method flush () = flush oc
    method close () = close oc
  end

let[@inline] map_char f oc = new map_char f oc
