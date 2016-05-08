structure Shell
:
sig
  val wd : unit   -> string
  val cd  : string -> string
  val ls  : unit   -> int
end
=
struct
  (* "wd" instead of "pwd" since it returns a string rather than printing *)
  fun wd ()   = OS.FileSys.getDir ()
  fun cd path = ( OS.FileSys.chDir path ; wd () )
  fun ls ()   = OS.Process.system "ls"
end
