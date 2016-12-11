structure OptionFunctor : FUNCTOR =
struct
    type 'a t = 'a option
    fun map f opt = case opt
                     of SOME a => SOME (f a)
                      | NONE   => NONE
end

structure OptionApplicativeFunctor =
struct
    val pure = SOME
    fun app fOpt m = case fOpt of SOME f => OptionFunctor.map f m
                                | NONE   => NONE

    fun seq fOpt m = case fOpt of SOME _ => m
                                | NONE   => NONE
end

functor OptionMonoid (M:MONOID) : MONOID =
struct
    type t = M.t option

    val e = NONE
    infix 3 **
    fun fa ** fb = case (fa,     fb    )
                    of  (SOME a, SOME b) => SOME (M.** (a, b))
                     |  (SOME _, NONE  ) => fa
                     |  (NONE,   SOME _) => fb
                     |  (NONE,   NONE  ) => NONE
end

structure OptionMonad : MONAD = struct
    type 'a t = 'a option
    fun ret x = SOME x
    fun bnd m k = case m
                   of SOME x => k x
                    | NONE   => NONE
end
