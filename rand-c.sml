<<<<<<< HEAD
(* Provides random operations on collections that can be turned to and from
   lists. Note that the permute function is only meaningful on arbitrarily
   orderable collections. Any collection with a fixed determinate order, or
   which is orderless by nature, will be unchanged after permutation; e.g.,
   ordered sets, unordered sets, etc. *)
=======
(* Provides random operations on collections that can be turned to
   and from lists. Note that the permute function is only meaningful
   on arbitrarily ordered collections. Any collection with a determinate
   order, or which is orderless by nature, will be unchanged after
   permutation; e.g., ordered sets, etc. *)
>>>>>>> sml-utils/master

signature RAND_COLLECTION =
sig
    type item
    type collection
    exception RandomCollection

    val member  : collection -> item
    val remove  : collection -> item * collection
    val permute : collection -> collection
end

signature LISTABLE =
sig
    type item
    type collection
    val toList : collection -> (item list)
    val fromList : item list -> collection
end

signature RAND_C =
sig
  functor List (type item) : RAND_COLLECTION
<<<<<<< HEAD
  functor Vector (type item) : RAND_COLLECTION
  functor Listable (L:LISTABLE) : RAND_COLLECTION
=======
  functor Listable (L:LISTABLE) : RAND_COLLECTION
  functor Vector (type item) : RAND_COLLECTION
>>>>>>> sml-utils/master
  functor OrdSet (O:ORD_SET) : RAND_COLLECTION
end

structure RandC : RAND_C =
struct

  functor List (type item) : RAND_COLLECTION =
  struct

    type item = item
    type collection = item list
    exception RandomCollection

<<<<<<< HEAD
    (***** TODO: parameterize seed so that any sort of seed can be passed in *)
=======
    (***** TBD: parameterize seed so that any sort of seed can be passed in *)
>>>>>>> sml-utils/master
    fun intFromTime () : int =
      let val seconds = (Time.toSeconds o Time.now ) ()
          val (n, _)  = IntInf.divMod (seconds, 2)
      in  LargeInt.toInt n
      end

    val seed = intFromTime ()

    val randGenerator = Random.rand (seed, seed div 2)

    fun randomIndex ls =
      let val maxIndex  = (List.length ls) - 1
      in  Random.randRange (0, maxIndex) randGenerator
      end

    fun member ls = List.nth (ls, randomIndex ls)

    fun removeNth ls n =
      let val remnant = List.take (ls,n) @ List.drop (ls,n+1)
          val removed = List.nth (ls, n)
      in (removed, remnant)
      end

    fun remove ls = if List.null ls
                    then raise RandomCollection
                    else removeNth ls (randomIndex ls)

    fun permute ls = if List.null ls
                     then []
                     else let val (removed, remnant) = remove ls
                          in removed :: (permute remnant)
                          end

  end

  functor Listable (L:LISTABLE) : RAND_COLLECTION =
  struct

    type item       = L.item
    type collection = L.collection

    exception RandomCollection

    structure R = List (type item = item)

    val member = (R.member o L.toList)
    fun remove c =
      let val (e, ls) = (R.remove o L.toList) c
      in  (e, L.fromList ls)
      end
    val permute = (L.fromList o R.permute o L.toList)

  end

  functor OrdSet (S:ORD_SET) : RAND_COLLECTION =
  struct

    structure L : LISTABLE =
    struct
      type item       = S.item
      type collection = S.set

      val toList      = S.listItems
      fun fromList ls = S.addList (S.empty, ls)
    end

    structure Rand = Listable (L)
    open Rand

  end

  functor Vector (type item) : RAND_COLLECTION =
  struct

    structure V = Vector

    structure L : LISTABLE =
    struct
      type item       = item
      type collection = item V.vector

      val toList : (item V.vector) -> item list =
       fn v => V.foldr List.:: [] v

      val fromList : item list -> item V.vector =
       fn v => V.fromList v
    end

    structure Rand = Listable (L)
    open Rand

  end

end

