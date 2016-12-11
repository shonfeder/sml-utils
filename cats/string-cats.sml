
functor StringMonoid (val prod : string * string -> string) : MONOID =
struct
    type t = string
    val e = ""
    infix 3 **
    val op** = prod
end
