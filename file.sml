(* Functions that operation on and represent files. Not focused on IO.
   For IO see structure IOU *)

(* TODO: Should I change this out for a posix or other *nixy approach? *)

structure File =
struct

    (* datatype file

      Eventually, this should specify a value that includes an exhaustive list of all
      information associated with a file in standard *nix file systems. For now, it
      just includes the information given by `ls -l`:

      file mode, number of links, owner name, group name, number of bytes in the
      file, abbreviated month, day-of-month file was last modified, hour file last
      modified, minute file last modified, and the pathname.

      E.g., `drwxr-xr-x@  4 sf  staff  136 Jul 10 21:15:49 2016 tests`
    *)

    (* structure FileSys = OS.FileSys *)

    (* type file_mode = int *)
    (* type bytes = int *)
    (* datatype path = Path of {arcs:string list, isAbs:bool, vol:string} *)

    (* datatype file = File of *)
    (*                 { file_mode : file_mode, *)
    (*                   number_of_links : int, *)
    (*                   owner : string, *)
    (*                   group : string, *)
    (*                   size  : bytes, *)
    (*                   modified : Time.time, *)
    (*                   path : path *)
    (*                 } *)

    (* (** Builds a file descriptor, describing a *nix file object *) *)

    (* fun descriptor name = *)
    (*   let *)
    (*       val absPath = FileSys.fullPath name *)
    (*       val file_mode =  *)
    (*       val modified = FileSys.modTime absPath *)
    (*       val path = OS.Path.fromString absPath *)
    (*       (* val size =  *) *)
    (*   in *)
    (*       true *)
    (*   end *)

end (* File *)
