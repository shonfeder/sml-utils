signature PARSEABLE =
sig
    type t
    val fromString : string -> t
end

signature PRINTABLE =
sig
    type t
    val toString : t -> string
end

signature PARSE_AND_PRINTABLE =
sig
    include PARSEABLE
    include PRINTABLE
end

structure StringPlus =
struct
    fun toWords str =
      let
          fun whiteSpc (#" " | #"\n" | #"\t" | #"\r") = true
            | whiteSpc _ = false
      in
          String.tokens whiteSpc str
      end
end
