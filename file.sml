(* Functions that operation on files, with an emphasis on convenience
   of IO *)

signature READ =
sig
    val lines : string -> string list
end

signature FILE =
sig
  structure Read : READ
end

structure File =
struct

  structure Read =
  struct
    fun lines f =
      let
          val stream = TextIO.openIn f
          fun dropNewline s = String.substring (s, 0, (String.size s) - 1)
          fun readLines () =
            case TextIO.inputLine stream
             of SOME line => (dropNewline line) :: readLines ()
              | NONE      => []
      in
          readLines ()
      end
  end

end
