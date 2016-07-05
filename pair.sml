signature PAIR =
sig
  val product : ('a -> 'b) * ('a -> 'c) -> 'a -> ('b * 'c)
  val map     : ('a -> 'c) -> ('a * 'a) -> ('c * 'c)
  val bimap   : (('a -> 'c) * ('b -> 'd)) -> ('a * 'b) -> ('c * 'd)
  val wrap    : 'a -> 'b -> ('a * 'b)
  val fst     : ('a * 'b) -> 'a
  val snd     : ('a * 'b) -> 'b
  val sort    : ('a * 'a -> order) -> ('a * 'a) -> ('a * 'a)
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

end
