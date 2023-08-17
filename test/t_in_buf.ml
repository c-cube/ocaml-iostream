open Iostream
open OUnit2

let spf = Printf.sprintf

let t1 =
  "of_bytes" >:: fun _ctx ->
  let ic = In_buf.of_bytes (Bytes.of_string "hello world!") in
  assert_equal (String.length "hello world!") ic.len;
  assert_equal "hello world!" (Bytes.sub_string ic.buf ic.off ic.len);
  In_buf.consume ic 5;
  assert_equal (String.length " world!") ic.len;
  assert_equal " world!" (Bytes.sub_string ic.buf ic.off ic.len);
  In_buf.consume ic 7;
  In_buf.fill_buffer ic;
  assert_equal 0 ic.len;
  assert_equal "" (Bytes.sub_string ic.buf ic.off ic.len);
  ()

let t2 =
  "read all form large file" >:: fun ctx ->
  let path, oc = OUnit2.bracket_tmpfile ~prefix:"t_in_buf" ~suffix:".txt" ctx in
  (* prepare file *)
  let content = List.init 20 (fun _ -> "lorem ipsum") |> String.concat "," in
  for _i = 1 to 1_000 do
    output_string oc content
  done;
  flush oc;
  close_out oc;

  let all_content = In_buf.with_open_file path In_buf.input_all in
  assert_equal ~printer:(spf "%d")
    (1_000 * String.length content)
    (String.length all_content);
  ()

let t3 =
  "read lines" >:: fun ctx ->
  let path, oc = OUnit2.bracket_tmpfile ~prefix:"t_in_buf" ~suffix:".txt" ctx in
  (* prepare file *)
  let content = List.init 20 (fun _ -> "lorem ipsum") |> String.concat "," in
  for _i = 1 to 1_000 do
    output_string oc content;
    output_char oc '\n'
  done;
  flush oc;
  close_out oc;

  let lines = In_buf.with_open_file path In_buf.input_lines in
  assert_equal
    ~printer:(fun l ->
      spf "[%s]" (String.concat ";" @@ List.map (spf "%S") @@ l))
    (List.init 1_000 (fun _ -> content))
    lines;
  ()

let suite = "in_buf" >::: [ t1; t2; t3 ]
let () = OUnit2.run_test_tt_main suite
