open IO

let decode ?(buf_size = 4096 * 32) (ic : In.t) : In.t = assert false
(* TODO:
   let buf = Bytes.create buf_size in
   let buf_len = ref 0 in
   let write_offset = ref 0 in
   let zlib_str = Zlib.inflate_init false in
   let is_done = ref false in
   let close () =
     Zlib.inflate_end zlib_str;
     In.close ic
   in
   let consume len : unit =
     if len > !buf_len then
       failwith
       @@ Printf.sprintf
            "inflate: error during decompression: invalid consume len %d (max \
             %d)"
            len !buf_len;
     write_offset := !write_offset + len
   in
   let fill_buf () : _ * _ * _ =
     (* refill [buf] if needed *)
     if !write_offset >= !buf_len && not !is_done then (
       let ib, ioff, ilen = In.fill_buf ic in
       try
         let finished, used_in, used_out =
           Zlib.inflate zlib_str ib ioff ilen buf 0 (Bytes.length buf)
             Zlib.Z_SYNC_FLUSH
         in
         In.consume ic used_in;
         write_offset := 0;
         buf_len := used_out;
         if finished then is_done := true
       with Zlib.Error (e1, e2) ->
         failwith
         @@ Printf.sprintf "inflate: error during decompression:\n%s %s" e1 e2
     );
     buf, !write_offset, !buf_len - !write_offset
   in
   In.of_funs ~fill_buf ~consume ~close ()
*)

let encode ?(buf_size = 4096 * 32) (oc : Out.t) : Out.t =
  let zlib_str = Zlib.deflate_init 4 false in

  let o_buf = Bytes.create buf_size in
  let o_off = ref 0 in
  let o_len = ref 0 in

  (* write output buffer to out *)
  let write_out (oc : Out.t) =
    if !o_len > 0 then (
      Out.output oc o_buf !o_off !o_len;
      o_off := 0;
      o_len := 0
    )
  in

  let flush_zlib ~flush (oc : Out.t) =
    let continue = ref true in
    while !continue do
      let finished, used_in, used_out =
        Zlib.deflate zlib_str Bytes.empty 0 0 o_buf 0 (Bytes.length o_buf) flush
      in
      assert (used_in = 0);
      o_len := !o_len + used_out;
      if finished then continue := false;
      write_out oc
    done
  in

  (* compress and consume input buffer *)
  let write_zlib ~flush (oc : Out.t) buf i len =
    let i = ref i in
    let len = ref len in
    while !len > 0 do
      let _finished, used_in, used_out =
        Zlib.deflate zlib_str buf !i !len o_buf 0 (Bytes.length o_buf) flush
      in
      i := !i + used_in;
      len := !len - used_in;
      o_len := !o_len + used_out;
      write_out oc
    done
  in

  let output buf i len = write_zlib ~flush:Zlib.Z_NO_FLUSH oc buf i len in

  let bchar = Bytes.create 1 in
  let output_char c =
    Bytes.set bchar 0 c;
    output bchar 0 1
  in

  let flush () =
    flush_zlib oc ~flush:Zlib.Z_FINISH;
    assert (!o_len = 0);
    oc.flush ()
  in
  let close () =
    flush ();
    Zlib.deflate_end zlib_str;
    oc.close ()
  in
  (* new output channel that compresses on the fly *)
  let oc' = Out.create ~flush ~close ~output ~output_char () in
  oc'

let encode_in ?(buf_size = 4096 * 32) (ic : In.t) : In.t = assert false
(* TODO:
   let refill = ref true in
   let buf = Bytes.create buf_size in
   let buf_len = ref 0 in
   let write_offset = ref 0 in
   let zlib_str = Zlib.deflate_init 4 false in
   let close () =
     Zlib.deflate_end zlib_str;
     In.close ic
   and consume n = write_offset := n + !write_offset
   and fill_buf () =
     let rec loop () =
       if !write_offset < !buf_len then
         ( (* still the same slice, not consumed entirely by output *)
           buf,
           !write_offset,
           !buf_len - !write_offset )
       else if not !refill then
         (* empty slice, no refill *)
         buf, !write_offset, !buf_len - !write_offset
       else (
         (* the output was entirely consumed, we need to do more work *)
         write_offset := 0;
         buf_len := 0;
         let in_s, in_i, in_len = In.fill_buf ic in
         if in_len > 0 then (
           (* try to decompress from input buffer *)
           let _finished, used_in, used_out =
             Zlib.deflate zlib_str in_s in_i in_len buf 0 (Bytes.length buf)
               Zlib.Z_NO_FLUSH
           in
           buf_len := used_out;
           In.consume ic used_in;
           if _finished then refill := false;
           loop ()
         ) else (
           (* finish sending the internal state *)
           let _finished, used_in, used_out =
             Zlib.deflate zlib_str in_s in_i in_len buf 0 (Bytes.length buf)
               Zlib.Z_FULL_FLUSH
           in
           assert (used_in = 0);
           buf_len := used_out;
           if used_out = 0 then refill := false;
           loop ()
         )
       )
     in
     try loop ()
     with Zlib.Error (e1, e2) ->
       failwith
       @@ Printf.sprintf "deflate: error during compression:\n%s %s" e1 e2
   in
   In.of_funs ~fill_buf ~consume ~close ()
*)
