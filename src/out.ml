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
        else (
          flush oc;
          close_out oc
        ));
    as_fd = (fun () -> Some (Unix.descr_of_out_channel oc));
  }

let of_unix_fd fd : t = of_out_channel (Unix.out_channel_of_descr fd)

let open_file ?(mode = 0o644) ?(flags = [ Unix.O_WRONLY; Unix.O_CREAT ])
    filename : t =
  let fd = Unix.openfile filename flags mode in
  of_unix_fd fd

let with_open_file ?mode ?flags filename f =
  let oc = open_file ?mode ?flags filename in
  Fun.protect ~finally:oc.close (fun () -> f oc)

let of_buffer (buf : Buffer.t) : t =
  {
    output_char = Buffer.add_char buf;
    output = Buffer.add_subbytes buf;
    flush = ignore;
    close = ignore;
    as_fd = (fun () -> None);
  }

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
  | Some fd -> ignore (Unix.lseek fd i Unix.SEEK_SET : int)
  | None -> raise (Sys_error "cannot seek")

let pos self : int =
  match self.as_fd () with
  | Some fd -> Unix.lseek fd 0 Unix.SEEK_CUR
  | None -> raise (Sys_error "cannot get pos")

let output_int self i =
  let s = string_of_int i in
  output_string self s

let output_lines self seq =
  Seq.iter
    (fun s ->
      output_string self s;
      output_char self '\n')
    seq

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

let map_char f (oc : t) : t =
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
