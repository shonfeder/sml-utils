signature PAIR =
sig
    (* access *)
    val product  : ('a -> 'b) * ('a -> 'c) -> 'a -> ('b * 'c)
    val map      : ('a -> 'c) -> ('a * 'a) -> ('c * 'c)
    val bimap    : (('a -> 'c) * ('b -> 'd)) -> ('a * 'b) -> ('c * 'd)
    val wrap     : 'a -> 'b -> ('a * 'b)
    val fst      : 'a * 'b -> 'a
    val snd      : 'a * 'b -> 'b
    val sort     : ('a * 'a -> order) -> ('a * 'a) -> ('a * 'a)
    val compare  : ('a * 'a -> order) -> ('a * 'a) * ('a * 'a) -> order

    (* info *)
    val memberEq : ''a -> (''a * ''a) -> bool
    val member   : ('a * 'a -> order) -> 'a -> ('a * 'a) -> bool
    (* pairs of booleans *)
    val both     : bool * bool -> bool
    val either   : bool * bool -> bool
    val nor      : bool * bool -> bool
    val nand     : bool * bool -> bool

    (* conversion *)
    val toString : ('a -> string) * ('b -> string) -> ('a * 'b) -> string
end

structure Pair : PAIR =
struct
    fun product (f, g) t = (f t, g t)
    fun map f (a, b)     = (f a, f b)
    fun bimap (f, g) (a, b) = (f a, g b)
    fun wrap a b = (a, b)
    fun fst (a, _) = a
    fun snd (_, b) = b

    fun sort compare' (a, b) = case compare' (a, b)
                                of GREATER => (b, a)
                                 | _       => (a, b)

    fun member compare' x (a, b) =
      compare' (x,a) = EQUAL orelse compare' (x,b) = EQUAL

    fun compare compare' ((a,b), (a',b')) =
      case (compare'(a,a'), compare' (b,b'))
       of (EQUAL, order) => order
        | (order, EQUAL) => order
        | (order, _    ) => order

    fun memberEq x (a, b) = (x = a orelse x = b)

    fun both   (a, b) = a andalso b
    fun either (a, b) = a orelse b
    fun nor    (a, b) = not (a orelse b)
    fun nand   (a, b) = not (a andalso b)

    fun toString toStrings pair = let val (a, b) = bimap toStrings pair
                                  in  "(" ^ a ^ "," ^ b ^ ")"
                                  end

end (* Pair *)

signature PAIR_SET =
sig
    type elem
    type t
    val * : elem * elem -> t
    val compare  : t * t -> order
    val member   : elem -> t -> bool
    val toPair   : t -> elem * elem
    val fromPair : elem * elem -> t
    val toString : (elem -> string) -> t -> string
end

functor PairSet (O:ORD_KEY) :> PAIR_SET where type elem = O.ord_key =
struct
    (* A type for unordered pairs *)
    type elem = O.ord_key
    datatype t = OrdPair of elem * elem
    infix 3 *

    fun (a * b) = (OrdPair o Pair.sort O.compare) (a,b)

    fun compare (OrdPair a, OrdPair b) = Pair.compare O.compare (a, b)
    fun member x (OrdPair pair) = Pair.member O.compare x pair
    fun toPair (OrdPair pair) = pair
    fun fromPair (a,b) = a * b
    fun toString toString' (OrdPair (a,b)) = "(" ^ (toString' a) ^ "x" ^ (toString' b) ^ ")"
end (* Ord *)
