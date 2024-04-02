open Iostream

open struct
  let default_buf_size = 16 * 1024
  let _default_comp_level = 4

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

class transduce_in_ ~mode (ic : #In_buf.t) : In.t =
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
              Zlib.Z_FINISH
          in
          assert (used_in = 0);
          if finished then state := Done;
          n_written := used_out
      done;
      !n_written
  end

let[@inline] decompress_in (ic : #In_buf.t) : In.t =
  new transduce_in_ ~mode:Inflate ic

let[@inline] compress_in ?(level = _default_comp_level) (ic : #In_buf.t) : In.t
    =
  new transduce_in_ ~mode:(Deflate level) ic

let decompress_in_buf ?buf_size ?buf (ic : #In_buf.t) : In_buf.t =
  let bytes = get_buf ?buf_size ?buf () in
  object
    (* use [transduce_in_] but hide its [input] method *)
    inherit transduce_in_ ~mode:Inflate ic as underlying

    (* use regular bufferized [input] *)
    inherit! In_buf.t_from_refill ~bytes ()

    method private refill (slice : Slice.t) =
      slice.len <- underlying#input slice.bytes 0 (Bytes.length slice.bytes)
  end

let compress_in_buf ?buf_size ?buf ?(level = _default_comp_level)
    (ic : #In_buf.t) : In_buf.t =
  let bytes = get_buf ?buf_size ?buf () in
  object
    (* use [transduce_in_] but hide its [input] method *)
    inherit transduce_in_ ~mode:(Deflate level) ic as underlying

    (* use regular bufferized [input] *)
    inherit! In_buf.t_from_refill ~bytes ()

    method private refill (slice : Slice.t) =
      slice.len <- underlying#input slice.bytes 0 (Bytes.length slice.bytes)
  end

(* write output buffer to out *)
let write_out (oc : #Out.t) (slice : Slice.t) : unit =
  if slice.len > 0 then (
    Out.output oc slice.bytes slice.off slice.len;
    slice.off <- 0;
    slice.len <- 0
  )

let transduce_out_ ?buf_size ?buf ~mode ~flush_out (oc : #Out.t) : Out_buf.t =
  let b1 = Bytes.create 1 in

  (* output buffer *)
  let slice_out =
    let bytes = get_buf ?buf_size ?buf () in
    Slice.of_bytes bytes
  in

  let zlib_str =
    match mode with
    | Inflate -> Zlib.inflate_init false
    | Deflate n -> Zlib.deflate_init n false
  in

  (* write nothing, but flush the internal state *)
  let flush_zlib ~flush (oc : #Out.t) =
    let continue = ref true in
    while !continue do
      slice_out.off <- 0;
      let finished, used_in, used_out =
        (match mode with
        | Inflate -> Zlib.inflate
        | Deflate _ -> Zlib.deflate)
          zlib_str Bytes.empty 0 0 slice_out.bytes 0
          (Bytes.length slice_out.bytes)
          flush
      in
      assert (used_in = 0);
      slice_out.len <- used_out;
      write_out oc slice_out;
      if finished || used_out = 0 then continue := false
    done;
    flush_out ()
  in

  (* compress and consume input buffer *)
  let write_zlib ~flush (oc : #Out.t) buf i len =
    let i = ref i in
    let len = ref len in
    while !len > 0 do
      write_out oc slice_out;
      let _finished, used_in, used_out =
        (match mode with
        | Inflate -> Zlib.inflate
        | Deflate _ -> Zlib.deflate)
          zlib_str buf !i !len slice_out.bytes 0
          (Bytes.length slice_out.bytes)
          flush
      in
      i := !i + used_in;
      len := !len - used_in;
      slice_out.len <- slice_out.len + used_out
    done;
    write_out oc slice_out
  in

  object
    method close () =
      flush_zlib oc ~flush:Zlib.Z_FINISH;
      assert (slice_out.len = 0);
      (match mode with
      | Inflate -> Zlib.inflate_end zlib_str
      | Deflate _ -> Zlib.deflate_end zlib_str);
      flush_out ();
      Out.close oc

    method output_char c =
      Bytes.set b1 0 c;
      write_zlib ~flush:Zlib.Z_NO_FLUSH oc b1 0 1

    method output buf i len = write_zlib ~flush:Zlib.Z_NO_FLUSH oc buf i len
    method flush () = flush_zlib ~flush:Zlib.Z_SYNC_FLUSH oc
  end

let compressed_out ?buf_size ?buf ?(level = _default_comp_level) (oc : #Out.t) :
    Out_buf.t =
  transduce_out_ ?buf_size ?buf ~flush_out:ignore ~mode:(Deflate level) oc

let compressed_out_buf ?buf_size ?buf ?(level = _default_comp_level)
    (oc : #Out_buf.t) : Out_buf.t =
  let flush_out () = Out_buf.flush oc in
  transduce_out_ ?buf_size ?buf ~flush_out ~mode:(Deflate level) (oc :> Out.t)

let decompressed_out ?buf_size ?buf oc : Out_buf.t =
  transduce_out_ ?buf_size ?buf ~flush_out:ignore ~mode:Inflate oc

let decompressed_out_buf ?buf_size ?buf (oc : #Out_buf.t) : Out_buf.t =
  let flush_out () = Out_buf.flush oc in
  transduce_out_ ?buf_size ?buf ~flush_out ~mode:Inflate (oc :> Out.t)
