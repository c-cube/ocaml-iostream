type t = {
  mutable buf: Buf.t;
  mutable off: int;
  fill_buffer: t -> int;
  close: unit -> unit;
}

let[@inline] get_bytes self = self.buf.bytes
let[@inline] get_off self = self.off
let[@inline] get_len self = Buf.size self.buf - self.off

(** Make sure [self.buf] is not empty if [ic] is not empty *)
let fill_buffer (self : t) : unit =
  if get_len self <= 0 then (
    Buf.clear self.buf;
    self.off <- self.fill_buffer self
  )

let create ?(buf = Buf.create ()) ?(close = ignore) ~fill_buffer () : t =
  { buf; off = 0; close; fill_buffer }

let input self b i len : int =
  fill_buffer self;
  let n_available = Buf.size self.buf - self.off in
  if n_available > 0 then (
    let n = min len n_available in
    Bytes.blit self.buf.bytes self.off b i n;
    self.off <- self.off + n;
    n
  ) else
    0

let[@inline] fill_and_get self =
  fill_buffer self;
  get_bytes self, get_off self, get_len self

let consume self n =
  assert (n <= get_len self);
  self.off <- self.off + n

let close self = self.close ()

let of_in ?buf ic : t =
  let close () = In.close ic in
  let fill_buffer self : int =
    assert (Buf.size self.buf = 0);
    In.input_into_buf ic self.buf;
    0
  in
  create ?buf ~close ~fill_buffer ()

let into_in (self : t) : In.t =
  let input b i len = input self b i len in
  let close () = close self in
  In.create ~close ~input ()

let copy_into (self : t) (oc : Out.t) : unit =
  let continue = ref true in
  while !continue do
    fill_buffer self;
    if self.buf.len = 0 then
      continue := false
    else
      Out.output oc self.buf.bytes 0 self.buf.len
  done
