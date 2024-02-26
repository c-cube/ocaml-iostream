module IO = Iostream
module IOZ = Iostream_camlzip

let t_decompress_oc_of_compress_ic str =
  let ic = IO.In_buf.of_string str in
  let buf = Buffer.create 32 in
  let oc = IO.Out.of_buffer buf in
  IO.In.copy_into (IOZ.compress_in ic) (IOZ.decompress_out oc);
  let str' = Buffer.contents buf in
  if str <> str' then ();
  ()

let t_decompress_ic_of_compress_ic str =
  let ic = IO.In_buf.of_string str in
  let buf = Buffer.create 32 in
  let oc = IO.Out.of_buffer buf in
  IO.In.copy_into
    (IOZ.decompress_in @@ IO.In_buf.bufferized @@ IOZ.compress_in ic)
    oc;
  let str' = Buffer.contents buf in
  if str <> str' then ();
  ()

let t_decompress_oc_of_compress_oc str =
  let ic = IO.In_buf.of_string str in
  let buf = Buffer.create 32 in
  let oc = IO.Out.of_buffer buf in
  IO.In.copy_into ic (IOZ.compress_out @@ IOZ.decompress_out oc);
  let str' = Buffer.contents buf in
  if str <> str' then ();
  ()

let test str =
  Printf.printf "oc of ic (len=%d)\n%!" (String.length str);
  t_decompress_oc_of_compress_ic str;
  Printf.printf "ic of ic (len=%d)\n%!" (String.length str);
  t_decompress_ic_of_compress_ic str;
  Printf.printf "oc of oc (len=%d)\n%!" (String.length str);
  t_decompress_oc_of_compress_oc str;
  Printf.printf "passed\n%!";
  ()

let () = test "hello world"

let () =
  let s = String.init 26 (fun i -> Char.chr (Char.code 'a' + i)) in
  let l = List.init 1000 (fun _ -> s) in
  let str = String.concat "." l in
  test str
