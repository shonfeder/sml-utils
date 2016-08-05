(* IOU = Input/Output Utilities *)

signature IO_UTIL =
sig
    (**
     * Returns a string from `stdin`, minus the last newline character.
     *)
    val getLn   : unit -> string
    (**
     * Prints a string to `stdout`.
     *)
    val printLn : string -> unit
end

structure IOU =
struct
    fun getLn () =
      let
          val inputStr = Option.getOpt (TextIO.inputLine TextIO.stdIn, "")
          val strSize  = String.size inputStr
          val strNoNewLine = String.substring (inputStr, 0, strSize - 1)
      in
          strNoNewLine
      end

    fun printLn s = print (s ^ "\n")
end
