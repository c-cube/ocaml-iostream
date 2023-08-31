class virtual t =
  object
    method virtual input : bytes -> int -> int -> int
    method close () = ()
  end

class virtual t_seekable =
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
    inherit t
    method input _ _ _ = 0
  end

let of_in_channel ?(close_noerr = false) (ic : in_channel) : t_seekable =
  object
    inherit t
    inherit Seekable.t
    method input buf i len = input ic buf i len

    method! close () =
      if close_noerr then
        close_in_noerr ic
      else
        close_in ic

    method seek i = seek_in ic i
    method pos () = pos_in ic
  end

let of_unix_fd ?(close_noerr = false) (fd : Unix.file_descr) : t_seekable =
  object
    inherit t
    method input buf i len = Unix.read fd buf i len

    method! close () =
      if close_noerr then (
        try Unix.close fd with _ -> ()
      ) else
        Unix.close fd

    method seek i = ignore (Unix.lseek fd i Unix.SEEK_SET : int)
    method pos () : int = Unix.lseek fd 0 Unix.SEEK_CUR
  end

let open_file ?(mode = 0o644) ?(flags = [ Unix.O_RDONLY ]) filename : t_seekable
    =
  let fd = Unix.openfile filename flags mode in
  of_unix_fd fd

let with_open_file ?mode ?flags filename f =
  let ic = open_file ?mode ?flags filename in
  Fun.protect ~finally:ic#close (fun () -> f ic)

let of_bytes ?(off = 0) ?len (b : bytes) : t_seekable =
  (* invariant: [!i + !len] is constant *)
  let i = ref off in
  let len0 =
    match len with
    | Some n ->
      if n > Bytes.length b - off then invalid_arg "Iostream.In.of_bytes";
      n
    | None -> Bytes.length b - off
  in
  let len = ref len0 in

  object
    inherit t
    inherit Seekable.t

    method input b_out i_out len_out =
      let n = min !len len_out in
      Bytes.blit b !i b_out i_out n;
      i := !i + n;
      len := !len - n;
      n

    method! close () = len := 0
    method pos () = !i

    method seek j =
      if j < off || j > off + len0 then
        raise (Sys_error "Iostream.In.see: invalid pos");
      i := j
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
