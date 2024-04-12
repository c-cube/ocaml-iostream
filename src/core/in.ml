open Common_

class type t =
  object
    method input : bytes -> int -> int -> int
    (** Read into the slice. Returns [0] only if the
        stream is closed. *)

    method close : unit -> unit
    (** Close the input. Must be idempotent. *)
  end

class type t_seekable =
  object
    inherit t
    inherit Seekable.t
  end

class type t_with_timeout =
  object
    inherit t
    method input_with_timeout : float -> bytes -> int -> int -> int
  end

let create ?(close = ignore) ~input () : t =
  object
    method close = close
    method input = input
  end

class empty : t =
  object
    method close () = ()
    method input _ _ _ = 0
  end

let empty = new empty

class of_in_channel ?(close_noerr = false) (ic : in_channel) : t_seekable =
  object
    method input buf i len = input ic buf i len

    method close () =
      if close_noerr then
        close_in_noerr ic
      else
        close_in ic

    method seek i = seek_in ic i
    method pos () = pos_in ic
  end

let[@inline] of_in_channel ?close_noerr ic = new of_in_channel ?close_noerr ic

class open_file ?close_noerr ?(mode = 0o644)
  ?(flags = [ Open_rdonly; Open_binary ]) filename : t_seekable =
  let ic = open_in_gen flags mode filename in
  of_in_channel ?close_noerr ic

let[@inline] open_file ?close_noerr ?mode ?flags filename =
  new open_file ?close_noerr ?mode ?flags filename

let with_open_file ?close_noerr ?mode ?flags filename f =
  let ic = open_file ?close_noerr ?mode ?flags filename in
  Fun.protect ~finally:ic#close (fun () -> f ic)

class of_bytes ?(off = 0) ?len (b : bytes) : t_seekable =
  (* i: current position in [b] *)
  let i = ref off in

  let len =
    match len with
    | Some n ->
      if n > Bytes.length b - off then invalid_arg "Iostream.In.of_bytes";
      n
    | None -> Bytes.length b - off
  in
  let end_ = off + len in

  object
    method input b_out i_out len_out =
      let n = min (end_ - !i) len_out in
      Bytes.blit b !i b_out i_out n;
      i := !i + n;
      n

    method close () = i := end_
    method pos () = !i - off (* pos starts at 0 *)

    method seek j =
      if j < 0 || j > len then raise (Sys_error "Iostream.In.seek: invalid pos");
      i := j + off
  end

let[@inline] of_bytes ?off ?len b = new of_bytes ?off ?len b

class of_string ?off ?len s : t_seekable =
  object
    inherit of_bytes ?off ?len (Bytes.unsafe_of_string s)
  end

let[@inline] of_string ?off ?len s = new of_string ?off ?len s

(** Read into the given slice.
      @return the number of bytes read, [0] means end of input. *)
let[@inline] input (self : #t) buf i len = self#input buf i len

(** Close the channel. *)
let[@inline] close self : unit = self#close ()

let rec really_input (self : #t) buf i len =
  if len > 0 then (
    let n = input self buf i len in
    if n = 0 then raise End_of_file;
    (really_input [@tailrec]) self buf (i + n) (len - n)
  )

let really_input_string self n : string =
  let buf = Bytes.create n in
  really_input self buf 0 n;
  Bytes.unsafe_to_string buf

let copy_into ?(buf = Bytes.create _default_buf_size) (ic : #t) (oc : #Out.t) :
    unit =
  let continue = ref true in
  while !continue do
    let len = input ic buf 0 (Bytes.length buf) in
    if len = 0 then
      continue := false
    else
      Out.output oc buf 0 len
  done

let concat (l0 : t list) : t =
  let ics = ref l0 in
  let rec input_rec b i len : int =
    match !ics with
    | [] -> 0
    | ic :: tl ->
      let n = ic#input b i len in
      if n > 0 then
        n
      else (
        ics := tl;
        input_rec b i len
      )
  in
  object
    method input bs i len = input_rec bs i len
    method close () = List.iter close l0
  end

class map_char f (ic : #t) : t =
  object
    method close () = close ic

    method input b i len : int =
      let n = ic#input b i len in
      if n > 0 then
        for j = i to i + n - 1 do
          let c = Bytes.get b j in
          (* safety: the index is valid because [get] above didn't raise. *)
          Bytes.unsafe_set b j (f c)
        done;
      n
  end

let[@inline] map_char f ic = new map_char f ic

let input_all_into_buffer self buf : unit =
  let oc = Out.of_buffer buf in
  copy_into self oc

let input_all ?(buf = Bytes.create 128) (self : #t) : string =
  let buf = ref buf in
  let i = ref 0 in

  let[@inline] full_ () = !i = Bytes.length !buf in

  let grow_ () =
    let old_size = Bytes.length !buf in
    let new_size = min Sys.max_string_length (old_size + (old_size / 4) + 10) in
    if old_size = new_size then
      failwith "input_all: maximum input size exceeded";
    let new_buf = Bytes.extend !buf 0 (new_size - old_size) in
    buf := new_buf
  in

  let rec loop () =
    if full_ () then grow_ ();
    let available = Bytes.length !buf - !i in
    let n = input self !buf !i available in
    if n > 0 then (
      i := !i + n;
      (loop [@tailrec]) ()
    )
  in
  loop ();

  if full_ () then
    Bytes.unsafe_to_string !buf
  else
    Bytes.sub_string !buf 0 !i

let[@inline] input_with_timeout (self : #t_with_timeout) t buf i len =
  self#input_with_timeout t buf i len
