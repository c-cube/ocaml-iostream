open Iostream
open OUnit2

let t1 =
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

let t2 =
  "in empty" >:: fun _ctx ->
  let ic = In.empty in
  let buf = Bytes.create 4 in
  assert_equal 0 (In.input ic buf 0 4)

let t3 =
  "transfer string to buf" >:: fun _ctx ->
  let ic = In.of_string ~len:1_500 (String.make 2_000 'a') in
  let buf = Buffer.create 32 in
  In.copy_into ic (Out.of_buffer buf);
  assert_equal 1_500 (Buffer.length buf);
  assert_equal (String.make 1_500 'a') (Buffer.contents buf)

let t4 =
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

let t5 =
  "big read" >:: fun _ctx ->
  let ic = In.of_string (String.make 2_000 'a') in
  let buf = Bytes.create 10_000 in
  let n = In.input ic buf 0 10_000 in
  assert_equal 2_000 n;
  let n = In.input ic buf 0 10_000 in
  assert_equal 0 n

let suite = "in" >::: [ t1; t2; t3; t4; t5 ]
let () = OUnit2.run_test_tt_main suite
