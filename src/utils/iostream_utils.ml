open Iostream

type filename = string

let with_in name f =
  let fd = Unix.openfile name [ Unix.O_RDONLY ] 0o644 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () -> f @@ In.of_unix_fd fd)

let with_in_l names f =
  let opened = ref [] in
  Fun.protect
    ~finally:(fun () -> List.iter Unix.close !opened)
    (fun () ->
      let l =
        List.map
          (fun name ->
            let fd = Unix.openfile name [ Unix.O_RDONLY ] 0o644 in
            opened := fd :: !opened;
            In.of_unix_fd fd)
          names
      in
      f l)

let with_out name f =
  let fd = Unix.openfile name [ Unix.O_WRONLY; Unix.O_CREAT ] 0o644 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      let oc = Out.of_unix_fd fd in
      let x = f oc in
      Out.flush oc;
      x)

let concat (l0 : In.t list) : In.t =
  let l = ref l0 in
  let rec input b i len : int =
    match !l with
    | [] -> 0
    | ic :: tl ->
      let n = ic.input b i len in
      if n > 0 then
        n
      else (
        l := tl;
        input b i len
      )
  in
  let close () = List.iter In.close l0 in
  In.create ~close ~input ()

let tee (l : Out.t list) : Out.t =
  let output bs i len = List.iter (fun oc -> Out.output oc bs i len) l
  and output_char c = List.iter (fun oc -> Out.output_char oc c) l
  and close () = List.iter Out.close l
  and flush () = List.iter Out.flush l in
  Out.create ~flush ~close ~output ~output_char ()

let read_all_into_buffer ic buf =
  let oc = Out.of_buffer buf in
  In.copy_into ic oc

let read_all_into_buf ic buf =
  let oc = Out.of_buf buf in
  In.copy_into ic oc

(** find index of [c] in slice, or raise [Not_found] *)
let index_in_slice_ bs i len c : int =
  let j = Bytes.index_from bs i c in
  if j < i + len then
    j
  else
    raise Not_found

let read_line (ic : In_buf.t) : string option =
  (* see if we can directly extract a line from current buffer *)
  let bs, i, len = In_buf.fill_and_get ic in
  if len = 0 then
    None
  else (
    match index_in_slice_ bs i len '\n' with
    | j ->
      (* easy case: buffer already contains a full line *)
      let line = Bytes.sub_string bs i (j - i) in
      In_buf.consume ic (j - i + 1);
      Some line
    | exception Not_found ->
      (* create new buffer, already filled with beginning of line *)
      let buf = Buffer.create 256 in
      Buffer.add_subbytes buf bs i len;
      In_buf.consume ic len;
      let continue = ref true in
      while !continue do
        let bs, i, len = In_buf.fill_and_get ic in
        if len = 0 then continue := false (* EOF *);
        match index_in_slice_ bs i len '\n' with
        | j ->
          Buffer.add_subbytes buf bs i (j - i);
          (* without '\n' *)
          In_buf.consume ic (j - i + 1);
          (* consume, including '\n' *)
          continue := false
        | exception Not_found ->
          Buffer.add_subbytes buf bs i len;
          In_buf.consume ic len
      done;
      Some (Buffer.contents buf)
  )

let map_in_c f (ic : In.t) : In.t =
  let close () = In.close ic in
  let input b i len : int =
    let n = ic.input b i len in
    if n > 0 then
      for j = i to i + n - 1 do
        Bytes.set b j (f (Bytes.get b j))
      done;
    n
  in

  In.create ~close ~input ()

let map_out_c f (oc : Out.t) : Out.t =
  let output_char c = Out.output_char oc (f c)
  and output buf i len =
    for j = i to i + len - 1 do
      Bytes.set buf j (f (Bytes.get buf j))
    done;
    Out.output oc buf i len
  and flush () = Out.flush oc
  and close () = Out.close oc in
  Out.create ~flush ~close ~output_char ~output ()

let read_lines ic =
  let rec loop l =
    match read_line ic with
    | None -> List.rev l
    | Some s -> loop (s :: l)
  in
  loop []

let read_lines' ic : _ list =
  let bic = In_buf.of_in ic in
  read_lines bic

let read_at_most ~close_rec max_size (ic : In_buf.t) : In.t =
  let remaining = ref max_size in

  let close () = if close_rec then In_buf.close ic in
  let input b_out i_out len_out =
    let b, i, len_in = In_buf.fill_and_get ic in
    let n = min len_in (min !remaining len_out) in

    if n > 0 then (
      Bytes.blit b i b_out i_out n;
      In_buf.consume ic n;
      remaining := !remaining - n
    );
    n
  in
  In.create ~close ~input ()

let read_all ic =
  let buf = Buffer.create 256 in
  read_all_into_buffer ic buf;
  Buffer.contents buf
