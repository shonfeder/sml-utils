signature FUNC_PAIR =
sig
    val f : 'a -> 'a -> 'c
    val g : 'b -> 'b -> 'd
end

structure PairMonoid (F:FUNC_PAIR) : MONOID =
struct
    (* type t = 'a * 'b *)

    (* val e = (Fn.id, Fn.id) *)
end
