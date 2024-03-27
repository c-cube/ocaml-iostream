open Iostream

module In : sig
  open In

  val of_unix_fd : ?close_noerr:bool -> Unix.file_descr -> t_seekable
  (** Create an in stream from a raw Unix file descriptor. The file descriptor
      must be opened for reading. *)
end = struct
  open In

  let of_unix_fd ?(close_noerr = false) (fd : Unix.file_descr) : t_seekable =
    object
      method input buf i len = Unix.read fd buf i len

      method close () =
        if close_noerr then (
          try Unix.close fd with _ -> ()
        ) else
          Unix.close fd

      method seek i = ignore (Unix.lseek fd i Unix.SEEK_SET : int)
      method pos () : int = Unix.lseek fd 0 Unix.SEEK_CUR
    end
end

module Out : sig
  open Out

  val of_unix_fd : ?close_noerr:bool -> Unix.file_descr -> t_seekable
  (** Output stream directly writing into the given Unix file descriptor. *)
end = struct
  open Out

  let of_unix_fd ?(close_noerr = false) fd : t_seekable =
    object
      method output buf i len =
        let i = ref i in
        let len = ref len in
        while !len > 0 do
          let n = Unix.write fd buf !i !len in
          i := !i + n;
          len := !len - n
        done

      method seek i = ignore (Unix.lseek fd i Unix.SEEK_SET : int)
      method pos () : int = Unix.lseek fd 0 Unix.SEEK_CUR

      method close () =
        if close_noerr then (
          try Unix.close fd with _ -> ()
        ) else
          Unix.close fd
    end
end

(** Sockets

    @since NEXT_RELEASE *)
module Sock : sig
  val of_unix_sock :
    ?close_noerr:bool ->
    Unix.file_descr ->
    Iostream.In.t_with_timeout * Iostream.Out.t
  (** [of_unix_sock sock] puts [sock] in non-blocking mode and
      returns a pair [ic, oc] of streams. *)
end = struct
  open Iostream

  let now_ = Unix.gettimeofday
  let short_timeout_ : float = 2.

  let of_unix_sock ?(close_noerr = false) (fd : Unix.file_descr) :
      In.t_with_timeout * Out.t =
    let closed = Atomic.make false in

    let close () =
      if not (Atomic.exchange closed true) then
        if close_noerr then (
          try Unix.close fd with _ -> ()
        ) else
          Unix.close fd
    in

    Unix.set_nonblock fd;

    let output_once buf i len =
      let n = ref 0 in
      while
        (not (Atomic.get closed))
        &&
        try
          n := Unix.write fd buf i len;
          false
        with
        | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
          (* sleep *)
          true
        | Unix.Unix_error ((Unix.ECONNRESET | Unix.ESHUTDOWN | Unix.EPIPE), _, _)
          ->
          (* exit *)
          false
      do
        ignore (Unix.select [] [ fd ] [] short_timeout_ : _ * _ * _)
      done;
      !n
    in

    let output buf i len =
      let i = ref i in
      let len = ref len in
      while !len > 0 do
        let n = output_once buf !i !len in
        if n = 0 then raise End_of_file;
        i := !i + n;
        len := !len - n
      done
    in

    let input_with_timeout t buf i len : int =
      let deadline = now_ () +. t in
      let n = ref 0 in
      while
        (not (Atomic.get closed))
        &&
        try
          n := Unix.read fd buf i len;
          false
        with
        | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
          (* sleep *)
          true
        | Unix.Unix_error ((Unix.ECONNRESET | Unix.ESHUTDOWN | Unix.EPIPE), _, _)
          ->
          (* exit *)
          false
      do
        let now = now_ () in
        if now >= deadline then raise Timeout;
        ignore (Unix.select [ fd ] [] [] (deadline -. now) : _ * _ * _)
      done;
      !n
    in

    let rec input buf i len =
      if Atomic.get closed then
        0
      else (
        match input_with_timeout short_timeout_ buf i len with
        | n -> n
        | exception Timeout -> input buf i len
      )
    in

    let ic =
      object
        method input_with_timeout = input_with_timeout
        method input = input
        method close = close
      end
    and oc =
      object
        method close = close
        method output = output
      end
    in
    ic, oc
end
