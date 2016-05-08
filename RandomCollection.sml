signature COLLECTION =
sig
  type 'a t
  val empty  : 'a t
  val null   : 'a t -> bool
  val size   : 'a t -> int
  val nth    : 'a t -> int -> 'a
  val delete : ''a -> ''a t -> ''a t
  val cons   : 'a -> 'a t -> 'a t
end

structure Collections =
struct
  structure Lists : COLLECTION =
  struct
    type 'a t = 'a list
    val empty       = []
    val null        = List.null
    val size        = List.length
    fun nth xs n    = List.nth (xs, n)
    fun delete x xs = List.filter (not o (fn y => y = x)) xs
    fun cons x xs   = (x :: xs)
  end

  (* functor Set (OK:ORD_KEY): COLLECTION = *)
  (* struct *)
  (* end *)
end

functor RandomCollection (C:COLLECTION) :>
sig
  val member  : 'a C.t  -> 'a
  val remove  : ''a C.t -> ''a * ''a C.t
  val permute : ''a C.t -> ''a C.t
end = struct

  exception RandomCollection

  val empty = C.empty

  (***** TBD: parameterize seed so that any sort of seed can be passed in *)
  fun intFromTime () : int =
    let val seconds = (Time.toSeconds o Time.now ) ()
        val (n, _)  = IntInf.divMod (seconds, 2)
    in  LargeInt.toInt n
    end

  val seed = intFromTime ()

  val randGenerator = Random.rand (seed, seed div 2)

  fun randomIndex ls =
    let val maxIndex  = (C.size ls) - 1
    in  Random.randRange (0, maxIndex) randGenerator
    end

  fun member ls = C.nth ls (randomIndex ls)

  fun remove ls = if C.null ls
                  then raise RandomCollection
                  else let val e       = member ls
                           val remnant = C.delete e ls
                       in (e, remnant)
                       end

  fun permute ls = if C.null ls
                   then C.empty
                   else let val (e, remnant) = remove ls
                        in C.cons e (permute remnant)
                        end

end

