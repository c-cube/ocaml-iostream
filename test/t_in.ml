open Iostream
open OUnit2

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
        In.of_string "hello";
        In.empty;
        In.of_string " ";
        In.of_string "world";
        In.of_string "!";
      ]
  in
  let r = In_buf.of_in i |> In_buf.input_all in
  assert_equal "hello world!" r

let t_map =
  "map" >:: fun _ctx ->
  let i = In.of_string "hello world!" |> In.map_char Char.uppercase_ascii in
  assert_equal "HELLO WORLD!" (In_buf.of_in i |> In_buf.input_all)

let suite =
  "in"
  >::: [
         t_of_str;
         t_empty;
         t_str_to_buf;
         t_close_str;
         t_big_read;
         t_concat;
         t_map;
       ]

let () = OUnit2.run_test_tt_main suite
