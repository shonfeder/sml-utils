signature PAIR =
sig
  val product : ('a -> 'b) * ('a -> 'c) -> 'a -> ('b * 'c)
  val map     : ('a -> 'c) -> ('a * 'a) -> ('c * 'c)
  val bimap   : (('a -> 'c) * ('b -> 'd)) -> ('a * 'b) -> ('c * 'd)
  val wrap    : 'a -> 'b -> ('a * 'b)
  val fst     : 'a * 'b -> 'a
  val snd     : 'a * 'b -> 'b
  val sort    : ('a * 'a -> order) -> ('a * 'a) -> ('a * 'a)
  val both    : bool * bool -> bool
  val either  : bool * bool -> bool
  val nor     : bool * bool -> bool
  val nand    : bool * bool -> bool
  functor Show ( val toStrings : ('a -> string) * ('b -> string) ) :
      sig
        val toString : ('a * 'b) -> string
      end
end

structure Pair : PAIR =
struct
  fun product (f, g) t = (f t, g t)
  fun map f (a, b)     = (f a, f b)
  fun bimap (f, g) (a, b) = (f a, g b)
  fun wrap a b = (a, b)
  fun fst (a, _) = a
  fun snd (_, b) = b
  fun sort compare (a, b) = case compare (a, b)
                             of (EQUAL | LESS) => (a, b)
                             |  GREATER        => (b, a)

  fun both   (a, b) = a andalso b
  fun either (a, b) = a orelse b
  fun nor    (a, b) = not (a orelse b)
  fun nand   (a, b) = not (a andalso b)

  functor Show (val toStrings : ('a -> string) * ('b -> string)) =
  struct
    fun toString pair = let val (a, b) = bimap toStrings pair
                        in  "(" ^ a ^ "," ^ b ^ ")"
                        end
  end

end
