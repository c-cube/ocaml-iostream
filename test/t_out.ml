open Iostream
open OUnit2

let spf = Printf.sprintf

let rot13 c =
  match c with
  | 'a' .. 'z' ->
    Char.chr (Char.code 'a' + ((Char.code c - Char.code 'a' + 13) mod 26))
  | 'A' .. 'Z' ->
    Char.chr (Char.code 'A' + ((Char.code c - Char.code 'A' + 13) mod 26))
  | c -> c

let t1 =
  "map_char" >:: fun _ctx ->
  let buf = Buffer.create 32 in
  let oc = Out.of_buffer buf |> Out.map_char rot13 in
  Out.output_string oc "hello";
  assert_equal ~printer:(spf "%S") "uryyb" (Buffer.contents buf);
  Out.output_string oc " world";
  assert_equal ~printer:(spf "%S") "uryyb jbeyq" (Buffer.contents buf);
  Buffer.clear buf;
  Out.output_string oc "!!";
  assert_equal ~printer:(spf "%S") "!!" (Buffer.contents buf);
  ()

let t2 =
  "with_out" >:: fun ctx ->
  let path, _oc = OUnit2.bracket_tmpfile ~prefix:"tout" ~suffix:"tmp" ctx in
  close_out_noerr _oc;
  Out.with_open_file path (fun out ->
      Out.output_string out "hello world";
      Out.output_char out '!');
  (* Printf.eprintf "done writing into %S\n%!" path; *)
  let content = In_buf.with_open_file path In_buf.input_all in
  assert_equal "hello world!" content

let suite = "out" >::: [ t1; t2 ]
let () = OUnit2.run_test_tt_main suite
