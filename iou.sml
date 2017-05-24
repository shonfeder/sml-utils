(* IOU = Input/Output Utilities *)

signature IOU_FILE_READ =
sig
    val lines : string -> string list
end

signature IOU_FILE =
sig
    structure Read : IOU_FILE_READ
end

signature IO_UTILS =
sig
    (**
     * Returns a string from `stdin`, minus the last newline character.
     *)
    val getLn   : unit -> string
    (**
     * Prints a string to `stdout`.
     *)
    val printLn : string -> unit
    structure File : IOU_FILE
end

structure IOU : IO_UTILS =
struct
    fun getLn () = Option.getOpt (TextIO.inputLine TextIO.stdIn, "")

    fun printLn s = print (s ^ "\n")

    structure File : IOU_FILE =
    struct

        structure Read : IOU_FILE_READ =
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

        end (* Read *)

    end (* File *)

end (* IOU *)
