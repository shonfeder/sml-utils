signature MONOID =
sig
    type t
    val ** : t * t -> t
    val e  : t
end

signature FUNCTOR =
sig
    type 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
end

signature MONAD = sig
    type 'a t
    val ret : 'a -> 'a t
    val bnd : 'a t -> ('a -> 'b t) -> 'b t
end
