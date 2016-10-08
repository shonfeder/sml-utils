(* With reference to
   - http://www.cs.cornell.edu/courses/cs312/2005sp/lectures/rec07.html
   - http://stackoverflow.com/questions/39200770/how-to-decide-whether-to-parameterize-on-the-type-level-or-the-module-level-when
 *)


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

    val fromList : 'a list  -> 'a stack
    val toList   : 'a stack -> 'a list
end

signature MONO_STACK =
sig
    type elem
    type t
    exception EmptyStack

    val empty : t
    val isEmpty : t -> bool

    val push : (elem * t) -> t
    val pop  : t -> t
    val top  : t -> elem
    val popTop : t -> t * elem

    val map : (elem -> elem) -> t -> t
    val app : (elem -> unit) -> t -> unit

    val fromList : elem list  -> t
    val toList   : t -> elem list
end

signature ELEM = sig type elem end

funsig MONO_STACK_FN (E:ELEM) = MONO_STACK where type elem = E.elem

structure Collect =
struct

    structure Stack  =
    struct
        structure Poly :> STACK
        = struct
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

            (* One advantage of the list-based implementation ;*)
            val fromList = Fn.id
            val toList   = Fn.id

        end (* Poly *)

        functor Mono (E:ELEM) : MONO_STACK where type elem = E.elem
                                             and type t = E.elem Poly.stack
        = struct
            type elem = E.elem
            type t = elem Poly.stack
            open Poly
        end (* Mono *)

    end (* Stack *)

end (* Collect *)


