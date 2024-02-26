open Slice
open Common_

class type t =
  object
    inherit In.t
    method fill_buf : unit -> Slice.t
    method consume : int -> unit
    method input : bytes -> int -> int -> int
  end

class virtual t_from_refill ?(buf = Slice.create _default_buf_size) () =
  object (self)
    val slice : Slice.t = buf
    method virtual private refill : Slice.t -> unit
    method close () = ()

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

let[@inline] consume (self : #t) n = self#consume n
let[@inline] fill_buf (self : #t) : Slice.t = self#fill_buf ()

let create ?(bytes = Bytes.create _default_buf_size) ?(close = ignore) ~refill
    () : t =
  let buf = Slice.of_bytes bytes in
  object
    inherit t_from_refill ~buf ()
    method! close () = close ()

    method private refill buf : unit =
      buf.off <- 0;
      buf.len <- refill buf.bytes
  end

let[@inline] input self b i len : int = self#input b i len
let[@inline] close self = self#close ()

class bufferized ?(bytes = Bytes.create _default_buf_size) (ic : #In.t) : t =
  let buf = Slice.of_bytes bytes in
  let eof = ref false in

  object
    inherit t_from_refill ~buf ()
    method! close () = ic#close ()

    method private refill buf =
      if not !eof then (
        buf.off <- 0;
        buf.len <- ic#input buf.bytes 0 (Bytes.length buf.bytes);
        if buf.len = 0 then eof := true
      )
  end

let[@inline] bufferized ?bytes ic = new bufferized ?bytes ic

class of_bytes ?(off = 0) ?len bytes =
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
    inherit t_from_refill ~buf ()

    method private refill buf =
      (* nothing to refill *)
      buf.off <- 0;
      buf.len <- 0
  end

let[@inline] of_bytes ?off ?len bs = new of_bytes ?off ?len bs

class of_string ?off ?len s =
  object
    inherit of_bytes ?off ?len (Bytes.unsafe_of_string s)
  end

let[@inline] of_string ?off ?len bs = new of_string ?off ?len bs

class of_in ?(bytes = Bytes.create _default_buf_size) ic =
  object
    inherit t_from_refill ~buf:(Slice.of_bytes bytes) ()
    method! close () = In.close ic

    method private refill buf =
      buf.off <- 0;
      buf.len <- In.input ic buf.bytes 0 (Bytes.length buf.bytes)
  end

let[@inline] of_in ?bytes ic = new of_in ?bytes ic

class of_in_channel ?bytes ic =
  object
    inherit of_in ?bytes (In.of_in_channel ic)
  end

let[@inline] of_in_channel ?bytes ic : t = new of_in_channel ?bytes ic

class open_file ?bytes ?mode ?flags filename : t =
  of_in ?bytes (In.open_file ?mode ?flags filename)

let[@inline] open_file ?bytes ?mode ?flags filename =
  new open_file ?bytes ?mode ?flags filename

let with_open_file ?bytes ?mode ?flags filename f =
  let ic = open_file ?bytes ?mode ?flags filename in
  Fun.protect ~finally:ic#close (fun () -> f ic)

let[@inline] into_in (self : #t) : In.t = (self :> In.t)
let input_all_into_buffer = In.input_all_into_buffer
let input_all = In.input_all

let copy_into (self : #t) (oc : #Out.t) : unit =
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

let input_line ?buffer (self : #t) : string option =
  (* see if we can directly extract a line from current buffer *)
  let slice = fill_buf self in
  if slice.len = 0 then
    None
  else (
    match Slice.find_index_exn slice '\n' with
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
        match Slice.find_index_exn bs '\n' with
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

let to_iter (self : #t) k : unit =
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

let to_seq (self : #t) : char Seq.t =
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
  (object
     inherit t_from_refill ~buf:(Slice.of_bytes bytes) ()

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
    :> t)
