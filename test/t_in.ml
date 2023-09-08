open Iostream
open OUnit2

let spf = Printf.sprintf

let t_of_str =
  "in1" >:: fun _ctx ->
  let ic = In.of_string "hello world" in
  let buf = Bytes.create 4 in
  assert_equal 4 (In.input ic buf 0 4);
  assert_equal "hell" (Bytes.to_string buf);
  assert_equal 4 (In.input ic buf 0 4);
  assert_equal "o wo" (Bytes.to_string buf);
  assert_equal 3 (In.input ic buf 0 3);
  assert_equal "rld" (Bytes.sub_string buf 0 3);
  assert_equal 0 (In.input ic buf 0 3);
  ()

let t_empty =
  "in empty" >:: fun _ctx ->
  let ic = In.empty in
  let buf = Bytes.create 4 in
  assert_equal 0 (In.input ic buf 0 4)

let t_str_to_buf =
  "transfer string to buf" >:: fun _ctx ->
  let ic = In.of_string ~len:1_500 (String.make 2_000 'a') in
  let buf = Buffer.create 32 in
  In.copy_into ic (Out.of_buffer buf);
  assert_equal 1_500 (Buffer.length buf);
  assert_equal (String.make 1_500 'a') (Buffer.contents buf)

let t_close_str =
  "close in string midway" >:: fun _ctx ->
  let ic = In.of_string (String.make 2_000 'a') in
  let buf = Bytes.create 100 in
  for _i = 1 to 10 do
    let n = In.input ic buf 0 100 in
    assert_equal 100 n
  done;
  In.close ic;
  let n = In.input ic buf 0 100 in
  assert_equal ~msg:"must be empty after close" 0 n

let t_big_read =
  "big read" >:: fun _ctx ->
  let ic = In.of_string (String.make 2_000 'a') in
  let buf = Bytes.create 10_000 in
  let n = In.input ic buf 0 10_000 in
  assert_equal 2_000 n;
  let n = In.input ic buf 0 10_000 in
  assert_equal 0 n

let t_concat =
  "concat" >:: fun _ctx ->
  let i =
    In.concat
      [
        (In.of_string "hello" :> In.t);
        In.empty;
        (In.of_string " " :> In.t);
        (In.of_string "world" :> In.t);
        (In.of_string "!" :> In.t);
      ]
  in
  let r = In_buf.of_in i |> In_buf.input_all in
  assert_equal "hello world!" r

let t_map =
  "map" >:: fun _ctx ->
  let i = In.of_string "hello world!" |> In.map_char Char.uppercase_ascii in
  assert_equal "HELLO WORLD!" (In_buf.of_in i |> In_buf.input_all)

let t_read_all =
  "read all form large file" >:: fun ctx ->
  let path, oc = OUnit2.bracket_tmpfile ~prefix:"t_in" ~suffix:".txt" ctx in
  (* prepare file *)
  let content = List.init 20 (fun _ -> "lorem ipsum") |> String.concat "," in
  for _i = 1 to 1_000 do
    output_string oc content
  done;
  flush oc;
  close_out oc;

  let all_content = In.with_open_file path In.input_all in
  assert_equal ~printer:(spf "%d")
    (1_000 * String.length content)
    (String.length all_content);
  ()

let t_seek =
  "seek in string" >:: fun _ctx ->
  let ic = In.of_string ~off:4 ~len:16 "oh hello world, how are you?" in
  assert_equal ~printer:string_of_int 0 (Seekable.pos ic);
  assert_equal ~printer:(spf "%S") "ello w" (In.really_input_string ic 6);
  assert_equal ~printer:string_of_int 6 (Seekable.pos ic);
  assert_equal ~printer:(spf "%S") "orld, how " (In.really_input_string ic 10);
  assert_equal ~printer:string_of_int 16 (Seekable.pos ic);
  assert_raises End_of_file (fun () -> In.really_input_string ic 1);
  (* seek back to 0 *)
  Seekable.seek ic 0;
  assert_equal ~printer:string_of_int 0 (Seekable.pos ic);
  assert_equal ~printer:(spf "%S") "ello w" (In.really_input_string ic 6);
  assert_equal ~printer:string_of_int 6 (Seekable.pos ic);
  assert_equal ~printer:(spf "%S") "orld, how " (In.really_input_string ic 10);

  ()

let suite =
  "in"
  >::: [
         t_of_str;
         t_empty;
         t_str_to_buf;
         t_close_str;
         t_big_read;
         t_concat;
         t_read_all;
         t_map;
         t_seek;
       ]

let () = OUnit2.run_test_tt_main suite
