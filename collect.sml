(* From http://www.cs.cornell.edu/courses/cs312/2005sp/lectures/rec07.html *)
signature STACK =
sig
    type 'a stack
    exception EmptyStack

    val empty : 'a stack
    val isEmpty : 'a stack -> bool

    val push : ('a * 'a stack) -> 'a stack
    val pop  : 'a stack -> 'a stack
    val top  : 'a stack -> 'a
    val popTop : 'a stack -> 'a stack * 'a

    val map : ('a -> 'b) -> 'a stack -> 'b stack
    val app : ('a -> unit) -> 'a stack -> unit
end

structure Collect =
struct

structure Stack :> STACK =
struct
    type 'a stack = 'a list
    exception EmptyStack

    val empty = []

    fun isEmpty st = null st

    fun push (x, st) = x::st

    fun pop []      = raise EmptyStack
      | pop (x::st) = st

    fun top []      = raise EmptyStack
      | top (x::st) = x

    fun popTop st = (pop st, top st)

    fun map f st = List.map f st
    fun app f st = List.app f st
end

end (* Collect *)
