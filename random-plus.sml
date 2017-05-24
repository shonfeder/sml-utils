signature RANDOM_GENERATOR =
sig
    val make : Random.rand
end (* RANDOM_GENERATOR *)

signature RANDOM_INT =
sig
    val range : int * int -> int
end (* RANDOM_INT *)

signature RANDOM_PLUS =
sig

structure Generator : RANDOM_GENERATOR
structure Int : RANDOM_INT

end (* RANDOM_PLUS *)

structure RandomPlus : RANDOM_PLUS =
struct

  structure Generator =
  struct

      fun intFromTime () : int =
        let val seconds = (Time.toSeconds o Time.now ) ()
            val (n, _)  = IntInf.divMod (seconds, 2)
        in  LargeInt.toInt n
        end

      val seed = intFromTime ()

      val make = Random.rand (seed, seed div 2)

  end (* Generator *)

  structure Int =
  struct

      val generator = Generator.make

      fun range (low,high) =
        Random.randRange (low, high) generator

  end (* RandomInt *)

end (* RandomPlus *)
