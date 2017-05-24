structure Shell
:
sig
  val cwd : string ref
  val wd  : unit   -> string
  val cd  : string -> string
  val ls  : unit   -> int
end
=
struct
  (* "wd" instead of "pwd" since it returns a string rather than printing *)
  val cwd = ref (OS.FileSys.getDir ())
  fun wd () = OS.FileSys.getDir ()
  fun cd path = ( OS.FileSys.chDir path ; cwd := wd () ; !cwd )
  fun ls ()   = OS.Process.system "ls"
end
