(* ordered sorts: ORDERED satisfies ORD_KEY *)

signature ORDERED =
sig
    include ORD_KEY
    type t = ord_key
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
      type t = ord_key
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

signature BOUNDED =
sig
    exception Bounded
    include ORDERED
    val minVal : ord_key
    val maxVal : ord_key
end

structure Bounded =
struct
    functor Make (O:ORDERED)
                 (val minVal : O.ord_key
                  val maxVal : O.ord_key)
            : BOUNDED =
    struct
        exception Bounded
        open O
        val minVal = minVal
        val maxVal = maxVal
    end
end


signature ENUMERABLE =
sig
    include ORDERED
    val succ : t -> t
    val pred : t -> t
    val interval : t * t -> t list
    val toInt : t -> int
    val fromInt : int -> t
end

signature BOUNDED_ENUMERABLE =
sig
    include BOUNDED
    val succ : t -> t
    val pred : t -> t
    val interval : t * t -> t list
    val toInt : t -> int
    val fromInt : int -> t
end


structure Enumerable =
struct

    functor Make (type t
                  val toInt : t -> int
                  val fromInt : int -> t)
            : ENUMERABLE =
    struct
        val toInt = toInt
        val fromInt = fromInt
        fun succ x = fromInt (toInt x + 1)
        fun pred x = fromInt (toInt x - 1)
        fun interval bounds =
          let val range = Pair.map toInt bounds
          in ListPlus.tabulateFrom range fromInt
          end
        structure O = Ordered.Make (type ord_key = t
                                    val compare = Int.compare o Pair.map toInt)
        open O
    end

    functor FromList (eqtype t
                      val items : t list)
            : BOUNDED_ENUMERABLE =
    struct
        structure V = Vector
        val vec = V.fromList items
        structure E = Make ( type t = t
                             fun fromInt n = V.sub(vec, n - 1)
                             fun toInt x = case V.findi (fn (_, y) => y = x) vec
                                            of NONE => raise Fail "Not the thing"
                                             | SOME (n, _) => n + 1 )
        structure B = Bounded.Make (E) ( val minVal = V.sub (vec, 0)
                                         val maxVal = V.sub (vec, V.length vec - 1) )
        open E
        open B
    end

    structure Char = Make (type t = Char.char val toInt = Char.ord val fromInt = Char.chr)
    structure Int  = Make (type t = Int.int   val toInt = Fn.id    val fromInt = Fn.id)

end

signature CYCLICAL =
sig
    include BOUNDED
    val succ : t -> t
    val prev : t -> t
end
structure Cyclical =
struct

    functor Make (E:BOUNDED_ENUMERABLE) (* : *)
    (* CYCLICAL *) =
    struct
        val minVal = E.minVal
        val maxVal = E.maxVal
        fun succ x = if E.equal (x, maxVal) then minVal else E.succ x
        fun pred x = if E.equal (x, minVal) then maxVal else E.pred x
        (* TODO periods : start, numItems *)
    end

end
