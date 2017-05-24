(* Provides random operations on collections that can be turned to and from
   lists. Note that the permute function is only meaningful on arbitrarily
   orderable collections. Any collection with a fixed determinate order, or
   which is orderless by nature, will be unchanged after permutation; e.g.,
   ordered sets, unordered sets, etc.

   TODO Special implementations of other common collection types, avoiding
        the overhead of list transformations.
 *)

signature POLY_RAND_COLLECTION =
sig
    type 'a t
    val member  : 'a t -> 'a
    val remove  : 'a t -> 'a * 'a t
    val permute : 'a t -> 'a t
end

signature MONO_RAND_COLLECTION =
sig
    type elem
    type t
    val member  : t -> elem
    val remove  : t -> elem * t
    val permute : t -> t
end

structure RandCollection =
struct
    structure List : POLY_RAND_COLLECTION =
    struct
        structure L = ListPlus
        structure Rand = RandomPlus
        type 'a t = 'a L.list

        fun randIndex []   = 0
          | randIndex list = Rand.Int.range (0, (length list) - 1)

        fun member list = L.nth (list, randIndex list)

        fun remove list =
          let val index = randIndex list
              fun remove' [] _ = raise Fail "removing from empty"
                | remove' (x::xs) i =
                if i = index
                then (x, xs)
                else Pair.bimap (Fn.id, Fn.curry op:: x) (remove' xs (i+1))
          in
              remove' list 0
          end

        fun permute list =
          if List.null list
          then []
          else let val (removed, remnant) = remove list
               in removed :: (permute remnant)
               end
    end (* List *)

    functor Make (C:LISTABLE) : MONO_RAND_COLLECTION where type elem = C.elem
                                                       and type t = C.t =
    struct
        type elem = C.elem
        type t    = C.t

        val member  = (List.member o C.toList)
        val remove  = (Pair.bimap (Fn.id, C.fromList) o List.remove  o C.toList)
        val permute = (C.fromList o List.permute o C.toList)

    end (* Make *)

    functor Set (O:ORD_KEY) (* : MONO_RAND_COLLECTION  *)=
    struct
        structure RandC = Make (Collection.Ordered.BinarySet (O))
        open RandC
    end (* Set *)
end (* RandCollection *)
