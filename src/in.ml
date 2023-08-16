type t = {
  input: bytes -> int -> int -> int;
  close: unit -> unit;
  as_fd: unit -> Unix.file_descr option;
}

let create ?(as_fd = fun _ -> None) ?(close = ignore) ~input () : t =
  { as_fd; close; input }

let empty : t = create ~input:(fun _ _ _ -> 0) ()

let of_in_channel ?(close_noerr = false) (ic : in_channel) : t =
  {
    input = (fun buf i len -> input ic buf i len);
    close =
      (fun () ->
        if close_noerr then
          close_in_noerr ic
        else
          close_in ic);
    as_fd = (fun () -> Some (Unix.descr_of_in_channel ic));
  }

let of_unix_fd ?(close_noerr = false) (fd : Unix.file_descr) : t =
  {
    input = (fun buf i len -> Unix.read fd buf i len);
    as_fd = (fun () -> Some fd);
    close =
      (fun () ->
        if close_noerr then (
          try Unix.close fd with _ -> ()
        ) else
          Unix.close fd);
  }

let of_bytes ?(off = 0) ?len (b : bytes) : t =
  (* invariant: [!i + !len] is constant *)
  let i = ref off in
  let len =
    ref
      (match len with
      | Some n ->
        if n > Bytes.length b - off then invalid_arg "Iostream.In.of_bytes";
        n
      | None -> Bytes.length b - off)
  in

  let input b_out i_out len_out =
    let n = min !len len_out in
    Bytes.blit b !i b_out i_out n;
    i := !i + n;
    len := !len - n;
    n
  in
  let close () = len := 0 in
  create ~close ~input ()

let of_string ?off ?len s : t = of_bytes ?off ?len (Bytes.unsafe_of_string s)

(** Read into the given slice.
      @return the number of bytes read, [0] means end of input. *)
let[@inline] input (self : t) buf i len = self.input buf i len

(** Close the channel. *)
let[@inline] close self : unit = self.close ()

let seek self i : unit =
  match self.as_fd () with
  | Some fd -> ignore (Unix.lseek fd (Int64.to_int i) Unix.SEEK_SET : int)
  | None -> raise (Sys_error "cannot seek")

let pos self : int64 =
  match self.as_fd () with
  | Some fd -> Int64.of_int (Unix.lseek fd 0 Unix.SEEK_CUR)
  | None -> raise (Sys_error "cannot get pos")

let input_into_buf (self : t) (buf : Buf.t) : unit =
  let n = self.input buf.bytes 0 (Bytes.length buf.bytes) in
  buf.len <- n

let copy_into ?(buf = Buf.create ~size:4_096 ()) (ic : t) (oc : Out.t) : unit =
  let continue = ref true in
  while !continue do
    let len = input ic buf.bytes 0 (Bytes.length buf.bytes) in
    if len = 0 then
      continue := false
    else
      Out.output oc buf.bytes 0 len
  done
