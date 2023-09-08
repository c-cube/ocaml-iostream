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

let dummy : t =
  object
    method flush () = ()
    method close () = ()
    method output_char _ = ()
    method output _ _ _ = ()
  end

(** [of_out_channel oc] wraps the channel into a {!Out_channel.t}.
      @param close_noerr if true, then closing the result uses [close_out_noerr]
      instead of [close_out] to close [oc] *)
let of_out_channel ?(close_noerr = false) (oc : out_channel) : t_seekable =
  object
    method output_char c = output_char oc c
    method output bs i len = output oc bs i len

    method close () =
      if close_noerr then
        close_out_noerr oc
      else (
        flush oc;
        close_out oc
      )

    method flush () = flush oc
    method seek i = seek_out oc i
    method pos () = pos_out oc
  end

let open_file ?close_noerr ?(mode = 0o644)
    ?(flags = [ Open_binary; Open_wronly; Open_creat; Open_trunc ]) filename :
    t_seekable =
  let oc = open_out_gen flags mode filename in
  of_out_channel ?close_noerr oc

let with_open_file ?close_noerr ?mode ?flags filename f =
  let oc = open_file ?close_noerr ?mode ?flags filename in
  Fun.protect ~finally:oc#close (fun () -> f oc)

let of_buffer (buf : Buffer.t) : t =
  object
    method close () = ()
    method flush () = ()
    method output_char c = Buffer.add_char buf c
    method output bs i len = Buffer.add_subbytes buf bs i len
  end

(** Output the buffer slice into this channel *)
let[@inline] output_char (self : #t) c : unit = self#output_char c

(** Output the buffer slice into this channel *)
let[@inline] output (self : #t) buf i len : unit = self#output buf i len

let[@inline] output_string (self : #t) (str : string) : unit =
  self#output (Bytes.unsafe_of_string str) 0 (String.length str)

let output_line (self : #t) (str : string) : unit =
  output_string self str;
  output_char self '\n'

(** Close the channel. *)
let[@inline] close self : unit = self#close ()

(** Flush (ie. force write) any buffered bytes. *)
let[@inline] flush self : unit = self#flush ()

let output_int self i =
  let s = string_of_int i in
  output_string self s

let output_lines self seq = Seq.iter (output_line self) seq

let tee (l : t list) : t =
  match l with
  | [] -> dummy
  | [ oc ] -> oc
  | _ ->
    let output bs i len = List.iter (fun oc -> output oc bs i len) l in
    let output_char c = List.iter (fun oc -> output_char oc c) l in
    let close () = List.iter close l in
    let flush () = List.iter flush l in
    create ~flush ~close ~output ~output_char ()

let map_char f (oc : #t) : t =
  let output_char c = output_char oc (f c) in
  let output buf i len =
    for j = i to i + len - 1 do
      let c = Bytes.get buf j in
      (* safety: [j] is valid because [get] above did not raise *)
      Bytes.unsafe_set buf j (f c)
    done;
    output oc buf i len
  in
  let flush () = flush oc in
  let close () = close oc in
  create ~flush ~close ~output_char ~output ()
