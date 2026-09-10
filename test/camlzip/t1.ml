module IO = Iostream
module IOZ = Iostream_camlzip

let t_decompress_oc_of_compress_ic str =
  let ic = IO.In_buf.of_string str in
  let buf = Buffer.create 32 in
  let oc = IO.Out.of_buffer buf in
  let dec = IOZ.decompressed_out oc in
  IO.In.copy_into (IOZ.compress_in ic) dec;
  IO.Out_buf.close dec;
  let str' = Buffer.contents buf in
  assert (str = str')

let t_decompress_oc_buf_of_compress_ic str =
  let ic = IO.In_buf.of_string str in
  let buf = Buffer.create 32 in
  let oc = IO.Out_buf.of_buffer buf in
  let dec = IOZ.decompressed_out_buf oc in
  IO.In.copy_into (IOZ.compress_in ic) dec;
  IO.Out_buf.close dec;
  let str' = Buffer.contents buf in
  assert (str = str')

let t_decompress_ic_of_compress_ic str =
  let ic = IO.In_buf.of_string str in
  let buf = Buffer.create 32 in
  let oc = IO.Out.of_buffer buf in
  IO.In.copy_into
    (IOZ.decompress_in @@ IO.In_buf.bufferized @@ IOZ.compress_in ic)
    oc;
  let str' = Buffer.contents buf in
  assert (str = str')

let t_decompress_oc_of_compress_oc str =
  let ic = IO.In_buf.of_string str in
  let buf = Buffer.create 32 in
  let oc = IO.Out.of_buffer buf in
  let pipe = IOZ.compressed_out @@ IOZ.decompressed_out oc in
  IO.In.copy_into ic pipe;
  IO.Out_buf.close pipe;
  let str' = Buffer.contents buf in
  assert (str = str')

let t_decompress_oc_buf_of_compress_buf str =
  let ic = IO.In_buf.of_string str in
  let buf = Buffer.create 32 in
  let oc = IO.Out_buf.of_buffer buf in
  let pipe = IOZ.compressed_out_buf @@ IOZ.decompressed_out oc in
  IO.In.copy_into ic pipe;
  IO.Out_buf.close pipe;
  let str' = Buffer.contents buf in
  assert (str = str')

let t_decompress_oc_buf_of_compress_buf_oc str =
  let ic = IO.In_buf.of_string str in
  let buf = Buffer.create 32 in
  let oc = IO.Out_buf.of_buffer buf in
  let pipe = IOZ.compressed_out_buf @@ IOZ.decompressed_out_buf oc in
  IO.In.copy_into ic pipe;
  IO.Out_buf.close pipe;
  let str' = Buffer.contents buf in
  assert (str = str')

(* exercises [decompress_in_buf] and [compress_in_buf] specifically, which the
   round-trips above do not. *)
let t_decompress_in_buf_of_compress_in_buf str =
  let ic = IO.In_buf.of_string str in
  let buf = Buffer.create 32 in
  let oc = IO.Out.of_buffer buf in
  IO.In.copy_into
    (IOZ.decompress_in_buf (IOZ.compress_in_buf ic) |> IO.In_buf.into_in)
    oc;
  let str' = Buffer.contents buf in
  assert (str = str')

let test str =
  Printf.printf "oc of ic (len=%d)\n%!" (String.length str);
  t_decompress_oc_of_compress_ic str;
  Printf.printf "oc_buf of ic (len=%d)\n%!" (String.length str);
  t_decompress_oc_buf_of_compress_ic str;
  Printf.printf "ic of ic (len=%d)\n%!" (String.length str);
  t_decompress_ic_of_compress_ic str;
  Printf.printf "oc of oc (len=%d)\n%!" (String.length str);
  t_decompress_oc_of_compress_oc str;
  Printf.printf "oc_buf of oc (len=%d)\n%!" (String.length str);
  t_decompress_oc_buf_of_compress_buf str;
  Printf.printf "oc_buf of oc_buf (len=%d)\n%!" (String.length str);
  t_decompress_oc_buf_of_compress_buf_oc str;
  Printf.printf "in_buf of in_buf (len=%d)\n%!" (String.length str);
  t_decompress_in_buf_of_compress_in_buf str;
  Printf.printf "passed\n%!";
  ()

let () = test "hello world"

let () =
  let s = String.init 26 (fun i -> Char.chr (Char.code 'a' + i)) in
  let l = List.init 1000 (fun _ -> s) in
  let str = String.concat "." l in
  test str

(* large input forces multiple [refill] passes through the 16 KiB buffer
   in the [_in_buf] variants. *)
let () =
  let s = String.init 64 (fun i -> Char.chr (Char.code '!' + (i mod 90))) in
  let l = List.init 4000 (fun _ -> s) in
  let str = String.concat "|" l in
  assert (String.length str > 64 * 1024);
  test str
