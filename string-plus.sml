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
