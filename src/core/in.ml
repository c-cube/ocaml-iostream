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

let create ?(close = ignore) ~input () : t =
  object
    method close = close
    method input = input
  end

let empty : t =
  object
    method close () = ()
    method input _ _ _ = 0
  end

let of_in_channel ?(close_noerr = false) (ic : in_channel) : t_seekable =
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

let open_file ?close_noerr ?(mode = 0o644)
    ?(flags = [ Open_rdonly; Open_binary ]) filename : t_seekable =
  let ic = open_in_gen flags mode filename in
  of_in_channel ?close_noerr ic

let with_open_file ?close_noerr ?mode ?flags filename f =
  let ic = open_file ?close_noerr ?mode ?flags filename in
  Fun.protect ~finally:ic#close (fun () -> f ic)

let of_bytes ?(off = 0) ?len (b : bytes) : t_seekable =
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

let of_string ?off ?len s : t_seekable =
  of_bytes ?off ?len (Bytes.unsafe_of_string s)

(** Read into the given slice.
      @return the number of bytes read, [0] means end of input. *)
let[@inline] input (self : #t) buf i len = self#input buf i len

(** Close the channel. *)
let[@inline] close self : unit = self#close ()

let copy_into ?(buf = Bytes.create 4_096) (ic : #t) (oc : Out.t) : unit =
  let continue = ref true in
  while !continue do
    let len = input ic buf 0 (Bytes.length buf) in
    if len = 0 then
      continue := false
    else
      Out.output oc buf 0 len
  done

let concat (l0 : t list) : t =
  let l = ref l0 in
  let rec input b i len : int =
    match !l with
    | [] -> 0
    | ic :: tl ->
      let n = ic#input b i len in
      if n > 0 then
        n
      else (
        l := tl;
        input b i len
      )
  in
  let close () = List.iter close l0 in
  create ~close ~input ()

let map_char f (ic : #t) : t =
  let close () = close ic in
  let input b i len : int =
    let n = ic#input b i len in
    if n > 0 then
      for j = i to i + n - 1 do
        let c = Bytes.get b j in
        (* safety: the index is valid because [get] above didn't raise. *)
        Bytes.unsafe_set b j (f c)
      done;
    n
  in
  create ~close ~input ()
