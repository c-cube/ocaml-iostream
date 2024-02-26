module IO = Iostream
module IOZ = Iostream_camlzip

let t_decompress_of_compress str =
  let ic = IO.In_buf.of_string str in
  let buf = Buffer.create 32 in
  let oc = IO.Out.of_buffer buf in
  IO.In.copy_into (IOZ.compress_in ic) (IOZ.decompress_out oc);
  let str' = Buffer.contents buf in
  if str <> str' then ();
  ()

let () = t_decompress_of_compress "hello world"

let () =
  let s = String.init 26 (fun i -> Char.chr (Char.code 'a' + i)) in
  let l = List.init 1000 (fun _ -> s) in
  let str = String.concat "." l in
  t_decompress_of_compress str
