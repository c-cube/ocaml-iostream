type t = {
  buf: bytes;
  mutable len: int;
  mutable off: int;
  fill_buffer: bytes -> int;
  close: unit -> unit;
}

let[@inline] get_bytes self = self.buf
let[@inline] get_off self = self.off
let[@inline] get_len self = self.len

(** Make sure [self.buf] is not empty if [ic] is not empty *)
let fill_buffer (self : t) : unit =
  if self.len <= 0 then (
    self.off <- 0;
    self.len <- self.fill_buffer self.buf
  )

let create ?(buf = Bytes.create 4_096) ?(close = ignore) ~fill_buffer () : t =
  { buf; len = 0; off = 0; close; fill_buffer }

let input self b i len : int =
  fill_buffer self;
  if self.len > 0 then (
    let n = min len self.len in
    Bytes.blit self.buf self.off b i n;
    self.off <- self.off + n;
    self.len <- self.len - n;
    n
  ) else
    0

let[@inline] fill_and_get self =
  fill_buffer self;
  get_bytes self, get_off self, get_len self

let consume self n =
  assert (n <= self.len);
  self.off <- self.off + n;
  self.len <- self.len - n

let[@inline] close self = self.close ()

let of_bytes ?(off = 0) ?len buf : t =
  let len =
    match len with
    | None -> Bytes.length buf - off
    | Some n ->
      if n > Bytes.length buf - off then
        invalid_arg "In_buf.of_bytes: invalid length";
      n
  in
  let fill_buffer _ = 0 in
  { buf; off; len; fill_buffer; close = ignore }

let of_in ?buf ic : t =
  let close () = In.close ic in
  let fill_buffer buf : int = In.input ic buf 0 (Bytes.length buf) in
  create ?buf ~close ~fill_buffer ()

let of_unix_fd ?buf ?close_noerr fd : t =
  of_in ?buf (In.of_unix_fd ?close_noerr fd)

let of_in_channel ?buf ic : t = of_in ?buf (In.of_in_channel ic)

let open_file ?buf ?mode ?flags filename : t =
  of_in ?buf (In.open_file ?mode ?flags filename)

let with_open_file ?buf ?mode ?flags filename f =
  let ic = open_file ?buf ?mode ?flags filename in
  Fun.protect ~finally:ic.close (fun () -> f ic)

let into_in (self : t) : In.t =
  let input b i len = input self b i len in
  let close () = close self in
  In.create ~close ~input ()

let copy_into (self : t) (oc : Out.t) : unit =
  let continue = ref true in
  while !continue do
    fill_buffer self;
    if self.len = 0 then
      continue := false
    else (
      Out.output oc self.buf 0 self.len;
      consume self self.len
    )
  done

let input_all_into_buffer self buf : unit =
  let oc = Out.of_buffer buf in
  copy_into self oc

let input_all ?buffer:(buf = Buffer.create 256) self : string =
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
  fill_buffer self;
  if self.len = 0 then
    None
  else (
    match index_in_slice_ self.buf self.off self.len '\n' with
    | j ->
      (* easy case: buffer already contains a full line *)
      let line = Bytes.sub_string self.buf self.off (j - self.off) in
      consume self (j - self.off + 1);
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

      Buffer.add_subbytes buf self.buf self.off self.len;
      consume self self.len;

      (* now read until we find ['\n'] *)
      let continue = ref true in
      while !continue do
        fill_buffer self;
        if self.len = 0 then continue := false (* EOF *);
        match index_in_slice_ self.buf self.off self.len '\n' with
        | j ->
          Buffer.add_subbytes buf self.buf self.off (j - self.off);
          (* without '\n' *)
          consume self (j - self.off + 1);
          (* consume, including '\n' *)
          continue := false
        | exception Not_found ->
          (* the whole [self.buf] is part of the current line. *)
          Buffer.add_subbytes buf self.buf self.off self.len;
          consume self self.len
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
    fill_buffer self;
    if self.len = 0 then
      continue := false
    else (
      for i = 0 to self.len - 1 do
        k (Bytes.get self.buf i)
      done;
      consume self self.len
    )
  done

let to_seq (self : t) : char Seq.t =
  let continue = ref true in
  let rec next () =
    if not !continue then
      Seq.Nil
    else if self.len = 0 then (
      fill_buffer self;
      next ()
    ) else (
      let c = Bytes.get self.buf self.off in
      consume self 1;
      Seq.Cons (c, next)
    )
  in
  next

let of_seq ?buf seq : t =
  let seq = ref seq in
  let fill_buffer buf =
    let rec loop idx =
      if idx >= Bytes.length buf then
        idx
      else (
        match !seq () with
        | Seq.Nil -> idx
        | Seq.Cons (c, seq_tl) ->
          seq := seq_tl;
          Bytes.set buf idx c;
          loop (idx + 1)
      )
    in
    loop 0
  in
  create ?buf ~fill_buffer ()
