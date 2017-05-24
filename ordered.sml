(* ordered sorts: ORDERED satisfies ORD_KEY *)

signature ORDERED =
sig
    include ORD_KEY
    (* type t *)
    val equal   : ord_key * ord_key -> bool
    val less    : ord_key * ord_key -> bool
    val greater : ord_key * ord_key -> bool

    val min : ord_key * ord_key -> ord_key
    val max : ord_key * ord_key -> ord_key
end

structure Ordered =
struct

    functor Make (O:ORD_KEY): ORDERED (* where type t = O.ord_key *) =
    struct
      open O
      (* type t = ord_key *)
      fun equal   pair = (compare pair = EQUAL)
      fun less    pair = (compare pair = LESS)
      fun greater pair = (compare pair = GREATER)

      fun min (a,b) = if less    (a,b) then a else b
      fun max (a,b) = if greater (a,b) then a else b
    end

    structure Int    = Make (struct open Int    type ord_key = Int.int       end)
    structure String = Make (struct open String type ord_key = String.string end)

    functor Pair (O:ORD_KEY) = Make (struct
                                        type ord_key = O.ord_key * O.ord_key
                                        val compare  = Pair.compare O.compare
                                      end) : ORDERED where type ord_key = O.ord_key * O.ord_key

end
