open Slice

let _default_buf_size = 4_096

class virtual cls ~buf =
  object (self)
    inherit In.cls
    val slice : Slice.t = buf
    method virtual refill : Slice.t -> unit

    method fill_buf () : Slice.t =
      if slice.len = 0 then self#refill slice;
      slice

    method consume (n : int) : unit = Slice.consume slice n
    (** Consume [n] bytes from the inner buffer. *)

    method input b i len : int =
      let buf = self#fill_buf () in

      if buf.len > 0 then (
        let n = min len buf.len in
        Bytes.blit buf.bytes buf.off b i n;
        Slice.consume buf n;
        n
      ) else
        0
    (** Default implementation of [input] using [fill_buf] *)
  end

class virtual cls_with_default_buffer =
  object
    inherit cls ~buf:(Slice.create _default_buf_size)
  end

type t = cls

let[@inline] consume (self : t) n = self#consume n
let[@inline] fill_buf (self : t) : Slice.t = self#fill_buf ()

let create ?(bytes = Bytes.create _default_buf_size) ?(close = ignore) ~refill
    () : t =
  let buf = Slice.of_bytes bytes in
  object
    inherit cls ~buf
    method! close () = close ()

    method refill buf : unit =
      buf.off <- 0;
      buf.len <- refill buf.bytes
  end

let[@inline] input self b i len : int = self#input b i len
let[@inline] close self = self#close ()

let of_bytes ?(off = 0) ?len bytes : t =
  let len =
    match len with
    | None -> Bytes.length bytes - off
    | Some n ->
      if n > Bytes.length bytes - off then
        invalid_arg "In_buf.of_bytes: invalid length";
      n
  in

  let buf = { bytes; off; len } in

  object
    inherit cls ~buf

    method refill buf =
      (* nothing to refill *)
      buf.off <- 0;
      buf.len <- 0
  end

let of_in ?(bytes = Bytes.create _default_buf_size) ic : t =
  object
    inherit cls ~buf:(Slice.of_bytes bytes)
    method! close () = In.close ic

    method refill buf =
      buf.off <- 0;
      buf.len <- In.input ic buf.bytes 0 (Bytes.length buf.bytes)
  end

let of_unix_fd ?bytes ?close_noerr fd : t =
  of_in ?bytes (In.of_unix_fd ?close_noerr fd)

let of_in_channel ?bytes ic : t = of_in ?bytes (In.of_in_channel ic)

let open_file ?bytes ?mode ?flags filename : t =
  of_in ?bytes (In.open_file ?mode ?flags filename)

let with_open_file ?bytes ?mode ?flags filename f =
  let ic = open_file ?bytes ?mode ?flags filename in
  Fun.protect ~finally:ic#close (fun () -> f ic)

let[@inline] into_in (self : t) : In.t = (self :> In.cls)

let copy_into (self : t) (oc : Out.t) : unit =
  let continue = ref true in
  while !continue do
    let buf = fill_buf self in
    if buf.len = 0 then
      continue := false
    else (
      Out.output oc buf.bytes 0 buf.len;
      consume self buf.len
    )
  done

let input_all_into_buffer self buf : unit =
  let oc = Out.of_buffer buf in
  copy_into self oc

let input_all ?buffer:(buf = Buffer.create 32) self : string =
  Buffer.clear buf;
  input_all_into_buffer self buf;
  Buffer.contents buf

(** find index of [c] in slice, or raise [Not_found] *)
let index_in_slice_ bs i len c : int =
  let j = Bytes.index_from bs i c in
  if j < i + len then
    j
  else
    raise Not_found

let input_line ?buffer (self : t) : string option =
  (* see if we can directly extract a line from current buffer *)
  let slice = fill_buf self in
  if slice.len = 0 then
    None
  else (
    match index_in_slice_ slice.bytes slice.off slice.len '\n' with
    | j ->
      (* easy case: buffer already contains a full line *)
      let line = Bytes.sub_string slice.bytes slice.off (j - slice.off) in
      consume self (j - slice.off + 1);
      Some line
    | exception Not_found ->
      (* Need to re-fill [self.buf]. We must first create a new holding buffer,
         already filled with beginning of line. *)
      let buf =
        match buffer with
        | Some b ->
          Buffer.clear b;
          b
        | None -> Buffer.create 256
      in

      Buffer.add_subbytes buf slice.bytes slice.off slice.len;
      consume self slice.len;

      (* now read until we find ['\n'] *)
      let continue = ref true in
      while !continue do
        let bs = fill_buf self in
        if bs.len = 0 then continue := false (* EOF *);
        match index_in_slice_ bs.bytes bs.off bs.len '\n' with
        | j ->
          Buffer.add_subbytes buf bs.bytes bs.off (j - bs.off);
          (* without '\n' *)
          consume self (j - bs.off + 1);
          (* consume, including '\n' *)
          continue := false
        | exception Not_found ->
          (* the whole [self.buf] is part of the current line. *)
          Buffer.add_subbytes buf bs.bytes bs.off bs.len;
          consume self bs.len
      done;
      Some (Buffer.contents buf)
  )

let input_lines ?(buffer = Buffer.create 32) ic =
  let rec loop l =
    match input_line ~buffer ic with
    | None -> List.rev l
    | Some s -> loop (s :: l)
  in
  loop []

let to_iter (self : t) k : unit =
  let continue = ref true in
  while !continue do
    let bs = fill_buf self in
    if bs.len = 0 then
      continue := false
    else (
      for i = 0 to bs.len - 1 do
        k (Bytes.get bs.bytes i)
      done;
      consume self bs.len
    )
  done

let to_seq (self : t) : char Seq.t =
  let continue = ref true in
  let rec next () =
    if not !continue then
      Seq.Nil
    else (
      let slice = fill_buf self in
      if slice.len = 0 then (
        continue := false;
        Seq.Nil
      ) else (
        let c = Bytes.get slice.bytes slice.off in
        Slice.consume slice 1;
        Seq.Cons (c, next)
      )
    )
  in
  next

let of_seq ?(bytes = Bytes.create _default_buf_size) seq : t =
  let seq = ref seq in

  object
    inherit cls ~buf:(Slice.of_bytes bytes)

    method refill bs =
      let rec loop idx =
        if idx >= Bytes.length bs.bytes then
          idx
        else (
          match !seq () with
          | Seq.Nil -> idx
          | Seq.Cons (c, seq_tl) ->
            seq := seq_tl;
            Bytes.set bs.bytes idx c;
            loop (idx + 1)
        )
      in
      bs.off <- 0;
      bs.len <- loop 0
  end
