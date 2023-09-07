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

  val of_unix_fd : Unix.file_descr -> t_seekable
  (** Output stream directly writing into the given Unix file descriptor. *)
end = struct
  open Out

  let of_unix_fd fd : t_seekable = of_out_channel (Unix.out_channel_of_descr fd)
end
