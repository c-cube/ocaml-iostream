open Iostream

open struct
  let default_buf_size = 16 * 1024

  let get_buf ?buf_size ?buf () =
    match buf with
    | Some b -> b
    | None ->
      let size = Option.value ~default:default_buf_size buf_size in
      Bytes.create size

  type decompress_state =
    | In_progress
    | Consuming_rest
    | Done
end

type mode =
  | Inflate
  | Deflate of int

let transduce_in_ ~mode (ic : #In_buf.t) : In.t =
  let zlib_str =
    match mode with
    | Inflate -> Zlib.inflate_init false
    | Deflate lvl -> Zlib.deflate_init lvl false
  in
  let state = ref In_progress in
  object
    method close () =
      (match mode with
      | Inflate -> Zlib.inflate_end zlib_str
      | Deflate _ -> Zlib.deflate_end zlib_str);
      In.close ic

    method input buf i len =
      let n_written = ref 0 in

      while !n_written = 0 && !state != Done do
        match !state with
        | Done -> assert false
        | In_progress ->
          let islice = In_buf.fill_buf ic in
          if islice.len = 0 then
            state := Consuming_rest
          else (
            let finished, used_in, used_out =
              (match mode with
              | Inflate -> Zlib.inflate
              | Deflate _ -> Zlib.deflate)
                zlib_str islice.bytes islice.off islice.len buf i len
                Zlib.Z_NO_FLUSH
            in
            if finished then state := Done;
            In_buf.consume ic used_in;
            n_written := used_out
          )
        | Consuming_rest ->
          (* finish sending the internal state *)
          let islice = Slice.empty in
          let finished, used_in, used_out =
            (match mode with
            | Inflate -> Zlib.inflate
            | Deflate _ -> Zlib.deflate)
              zlib_str islice.bytes islice.off islice.len buf i len
              Zlib.Z_FULL_FLUSH
          in
          assert (used_in = 0);
          if finished then state := Done;
          n_written := used_out
      done;
      !n_written
  end

let[@inline] decompress_in (ic : #In_buf.t) : In.t =
  transduce_in_ ~mode:Inflate ic

let[@inline] compress_in ?(level = 4) (ic : #In_buf.t) : In.t =
  transduce_in_ ~mode:(Deflate level) ic

(* write output buffer to out *)
let write_out (oc : Out.t) (slice : Slice.t) : unit =
  if slice.len > 0 then (
    Out.output oc slice.bytes slice.off slice.len;
    slice.off <- 0;
    slice.len <- 0
  )

let transduce_out_ ?buf_size ?buf ~mode (oc : Out.t) : Out.t =
  let buf = get_buf ?buf_size ?buf () in
  let slice = Slice.of_bytes buf in
  let zlib_str =
    match mode with
    | Inflate -> Zlib.inflate_init false
    | Deflate n -> Zlib.deflate_init n false
  in

  (*
  let o_buf = Bytes.create buf_size in
  let o_off = ref 0 in
  let o_len = ref 0 in
  *)
  let flush_zlib ~flush (oc : Out.t) =
    let continue = ref true in
    while !continue do
      let finished, used_in, used_out =
        (match mode with
        | Inflate -> Zlib.inflate
        | Deflate _ -> Zlib.deflate)
          zlib_str Bytes.empty 0 0 slice.bytes 0 (Bytes.length slice.bytes)
          flush
      in
      assert (used_in = 0);
      slice.len <- used_out;
      write_out oc slice;
      if finished then continue := false
    done
  in

  (* compress and consume input buffer *)
  let write_zlib ~flush (oc : Out.t) buf i len =
    let i = ref i in
    let len = ref len in
    while !len > 0 do
      write_out oc slice;
      let _finished, used_in, used_out =
        (match mode with
        | Inflate -> Zlib.inflate
        | Deflate _ -> Zlib.deflate)
          zlib_str buf !i !len slice.bytes 0 (Bytes.length slice.bytes) flush
      in
      i := !i + used_in;
      len := !len - used_in;
      slice.len <- slice.len + used_out
    done;
    write_out oc slice
  in

  object
    method close () =
      flush_zlib oc ~flush:Zlib.Z_FINISH;
      assert (slice.len = 0);
      (match mode with
      | Inflate -> Zlib.inflate_end zlib_str
      | Deflate _ -> Zlib.deflate_end zlib_str);
      Out.close oc

    method output buf i len = write_zlib ~flush:Zlib.Z_NO_FLUSH oc buf i len
  end

let compress_out ?buf_size ?buf ?(level = 4) oc : Out.t =
  transduce_out_ ?buf_size ?buf ~mode:(Deflate level) oc

let decompress_out ?buf_size ?buf oc : Out.t =
  transduce_out_ ?buf_size ?buf ~mode:Inflate oc
