type t = {
  output_char: char -> unit;  (** Output a single char *)
  output: bytes -> int -> int -> unit;  (** Output slice *)
  flush: unit -> unit;  (** Flush underlying buffer *)
  close: unit -> unit;  (** Close the output. Must be idempotent. *)
  as_fd: unit -> Unix.file_descr option;
}

let create ?(as_fd = fun () -> None) ?(flush = ignore) ?(close = ignore)
    ~output_char ~output () : t =
  { as_fd; flush; close; output_char; output }

let dummy : t = create ~output_char:ignore ~output:(fun _ _ _ -> ()) ()

(** [of_out_channel oc] wraps the channel into a {!Out_channel.t}.
      @param close_noerr if true, then closing the result uses [close_out_noerr]
      instead of [close_out] to close [oc] *)
let of_out_channel ?(close_noerr = false) (oc : out_channel) : t =
  {
    output_char = (fun c -> output_char oc c);
    output = (fun buf i len -> output oc buf i len);
    flush = (fun () -> flush oc);
    close =
      (fun () ->
        if close_noerr then
          close_out_noerr oc
        else
          close_out oc);
    as_fd = (fun () -> Some (Unix.descr_of_out_channel oc));
  }

let of_unix_fd fd : t = of_out_channel (Unix.out_channel_of_descr fd)

let of_buffer (buf : Buffer.t) : t =
  {
    output_char = Buffer.add_char buf;
    output = Buffer.add_subbytes buf;
    flush = ignore;
    close = ignore;
    as_fd = (fun () -> None);
  }

let of_buf buf : t =
  let output_char = Buf.add_char buf in
  let output = Buf.add_bytes buf in
  create ~output_char ~output ()

(** Output the buffer slice into this channel *)
let[@inline] output_char (self : t) c : unit = self.output_char c

(** Output the buffer slice into this channel *)
let[@inline] output (self : t) buf i len : unit = self.output buf i len

let[@inline] output_string (self : t) (str : string) : unit =
  self.output (Bytes.unsafe_of_string str) 0 (String.length str)

(** Close the channel. *)
let[@inline] close self : unit = self.close ()

(** Flush (ie. force write) any buffered bytes. *)
let[@inline] flush self : unit = self.flush ()

let seek self i : unit =
  match self.as_fd () with
  | Some fd -> ignore (Unix.lseek fd (Int64.to_int i) Unix.SEEK_SET : int)
  | None -> raise (Sys_error "cannot seek")

let pos self : int64 =
  match self.as_fd () with
  | Some fd -> Int64.of_int (Unix.lseek fd 0 Unix.SEEK_CUR)
  | None -> raise (Sys_error "cannot get pos")

let output_buf (self : t) (buf : Buf.t) : unit =
  let b = Buf.bytes_slice buf in
  output self b 0 (Buf.size buf)

let output_int self i =
  let s = string_of_int i in
  output_string self s

let output_lines self seq =
  Seq.iter
    (fun s ->
      output_string self s;
      output_char self '\n')
    seq

(* etc. *)
