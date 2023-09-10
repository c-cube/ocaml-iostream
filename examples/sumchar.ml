(** Sum the char from stdin *)

open Iostream

let spf = Printf.sprintf
let ( let@ ) = ( @@ )

let () =
  (* let ic = In_buf.of_in_channel ~bytes:(Bytes.create @@ 64 * 1024) stdin in *)
  let ic =
    In_buf.of_in ~bytes:(Bytes.create @@ (64 * 1024))
    @@ Iostream_unix.In.of_unix_fd @@ Unix.stdin
  in

  let sum = ref 0 in
  let continue = ref true in
  while !continue do
    let slice = In_buf.fill_buf ic in
    if slice.len = 0 then continue := false;

    for i = 0 to slice.len - 1 do
      sum := !sum + int_of_char (Bytes.unsafe_get slice.bytes i)
    done;
    In_buf.consume ic slice.len
  done;
  Printf.printf "sum=%d\n%!" !sum
