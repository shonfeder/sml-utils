(* A common interface for collection types
 *
 * should provide means of specifying an interface common to
 * Lists, Sets, Stacks, Array, Vectors, Trees, etc.
 * Accomplishing this depends on finding the minimum set of
 * features essential to the idea of a collection.
 **)

(* Design decisions:

- whenever possible, functions should take the collection to operate on
  as the last argument, to facilitate composing chains of operations on
  the collection.

- default identity is determined by ordered types, not eq types.
  This is to facilitate interface with implementations of ORDERED and avoid
  the many problems with eqtypes.

- should include interfaces for a number of different kinds of collections:
  - polymorphic collections
  - monomorphic collections : general monomorphic collections
  - ordered collections     : monomorphic collections sealed around ordered types
  - mutable collections
 *)

signature ELEM = sig type elem end

signature LISTABLE =
sig
    type t
    type elem
    val toList   : t -> elem list
    val fromList : elem list -> t
end

signature POLY_COLLECTION =
sig
    type 'a t
    val empty : 'a t
    val isEmpty : 'a t -> bool
    val size : 'a t -> int

    val member : ('a * 'a -> order) -> 'a -> 'a t -> bool
    val remove : ('a * 'a -> order) -> 'a -> 'a t -> 'a t

    val add    : 'a -> 'a t -> 'a t

    val map : ('a -> 'b) -> 'a t -> 'b t

    val filter : ('a -> bool) -> 'a t -> 'a t

    val foldl : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b

    val fromList : 'a list -> 'a t
    val toList   : 'a t -> 'a list
end (* POLY_COLLECTION *)

signature MONO_COLLECTION_CORE =
sig
    type elem
    type t
    val empty : t
    val isEmpty : t -> bool
    val size : t -> int

    val add    : elem -> t -> t

    val map : (elem -> elem) -> t -> t

    val filter : (elem -> bool) -> t -> t

    val foldl : (elem * 'a -> 'a) -> 'a -> t -> 'a
    val foldr : (elem * 'a -> 'a) -> 'a -> t -> 'a

    val fromList : elem list -> t
    val toList   : t -> elem list
end (* MONO_COLLECTION_CORE *)

signature MONO_COLLECTION =
sig
    include MONO_COLLECTION_CORE

    val member : (elem * elem -> order) -> elem -> t -> bool
    val remove : (elem * elem -> order) -> elem -> t -> t
    val delete : (elem * elem -> order) -> elem -> t -> t

end (* MONO_COLLECTION *)

signature ORD_COLLECTION =
sig
    include MONO_COLLECTION_CORE

    val member : elem -> t -> bool
    val remove : elem -> t -> t
    val delete : elem -> t -> t
    val maxItem : t -> elem
    val minItem : t -> elem

end

funsig ORD_COLLECTION_FN (O:ORD_KEY) = ORD_COLLECTION where type elem = O.ord_key

signature ORD_COLLECTION_SET =
sig
    include ORD_COLLECTION
    val union : t * t -> t
end

funsig ORD_COLLECTION_SET_FN (O:ORD_KEY) = ORD_COLLECTION_SET where type elem = O.ord_key

structure Collection =
struct
    structure List : POLY_COLLECTION =
    struct
        open ListPlus
        datatype t = datatype ListPlus.list
        val isEmpty  = null
        val size     = length
        val fromList = Fn.id
        val toList   = Fn.id
    end (* List *)

    structure Ordered =
    struct
        functor List (O:ORD_KEY): ORD_COLLECTION where type elem = O.ord_key =
        struct
            open List
            type elem = O.ord_key
            type t = elem list
            structure Ord = Ordered.Make(O)
            val member = ListPlus.member Ord.compare
            val remove = ListPlus.remove Ord.compare
            fun delete x = ListPlus.filter (not o Fn.curry Ord.equal x)

            fun maxItem [] = raise Empty
              | maxItem (x::xs) = List.foldl Ord.max x xs
            fun minItem [] = raise Empty
              | minItem (x::xs) = List.foldl Ord.min x xs

        end (* List *)

        structure IntList = List (Ordered.Int)
        structure StrList = List (Ordered.String)

        functor BinarySet (O:ORD_KEY): ORD_COLLECTION_SET where type elem = O.ord_key =
        struct
            structure S = BinarySetFn(O)
            open S
            type elem = O.ord_key
            type t = S.set
            val size   = S.numItems
            val member = (Fn.curry o Fn.flip) S.member
            val remove = (Fn.curry o Fn.flip) S.delete
            val delete = remove
            val add = (Fn.curry o Fn.flip) S.add

        end (* BinarySet *)

        structure IntSet = BinarySet(Ordered.Int)
        structure StrSet = BinarySet(Ordered.String)
    end (* Ordered *)

    functor Convert (A:LISTABLE) (B:LISTABLE where type elem = A.elem) =
    struct
        val >> = (B.fromList o A.toList)
        val << = (A.fromList o B.toList)
    end

end (* Collection *)
