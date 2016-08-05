signature PROGRAM =
sig
    type data
    type msg = string

    (**
     * Defines the prompt used in the REPL
     *)
    val prompt   : string

    (**
    * Parse a string into program data or an error message.
    *)
    val parse    : string -> (msg, data) Either.either

    (**
     * Eval program data into different data, or an error message.
     *)
    val eval     : data   -> (msg, data) Either.either

    (**
     * Convert program data into a string.
     *)
    val toString : data   -> string
end

signature REPL =
sig
    type data

    val read  : unit -> data
    val eval  : data -> data
    val print : data -> unit
    val start : unit -> unit
end

functor Repl (P:PROGRAM) : REPL =
struct
    type data = P.data
    type msg  = P.msg

    val prompt = IOU.printLn P.prompt

    (* TODO: make return either value instead of just exiting? *)
    fun command ":q" = OS.Process.exit 0
      | command str  = str

    fun read () : data =
      (errOrData o P.parse o command o IOU.getLn) prompt
    and errOrData err_or_data : data =
        case err_or_data
         of Either.INL msg  => ( IOU.printLn msg ; read () )
          | Either.INR data => data

    val eval  : data -> data = errOrData o P.eval
    val print : data -> unit = IOU.printLn o P.toString

    fun loop () = (loop o print o eval o read) ()

    fun start () = loop ()
end

(* A test structure *)
structure EchoProgram : PROGRAM =
struct
type data = string
type msg  = string
type either = (msg, data) Either.either

val prompt : string = "Echo > "
val parse    : string -> either = Either.INR
val eval     : data   -> either = Either.INR o Fn.id
val toString : data   -> string = Fn.id
end (* ReplEcho *)

structure EchoRepl = Repl(EchoProgram:PROGRAM)
