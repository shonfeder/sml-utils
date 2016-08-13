(** NUMBERS
 * Provides a common, core interface for number modules.
 * It is meant to allow for modular parameterization over
 * different number types. *)

signature NUMBER =
sig
    type num
    val + : num * num -> num
    val - : num * num -> num
    val * : num * num -> num
    val / : num * num -> num
    val pow : num * num -> num
    val abs : num -> num

    val min : num * num -> num
    val max : num * num -> num
    val < : num * num -> bool
    val <= : num * num -> bool
    val > : num * num -> bool
    val >= : num * num -> bool
    val == : num * num -> bool
    val compare : num * num -> order

    val toString : num -> string
    val fromString : string -> num option
end

signature NUMBERS =
sig
    structure Real : NUMBER
    structure Int  : NUMBER
end

structure Numbers : NUMBERS =
struct

    structure Real =
    struct
        open Real
        type num = real
        val pow = Math.pow
    end

    structure Int =
    struct

       open Int
       type num = int

       exception IntNumberNegativeExp

       val (op /)  = (op div)
       val (op ==) = (op =)
       fun pow (_,0)   = 1
         | pow (n,1)   = n
         | pow (n,exp) = if sign exp = 1
                         then n * n * (pow (n,exp-1))
                         else raise IntNumberNegativeExp
    end
    (* structure Int  = Int  : NUMBER *)
end
