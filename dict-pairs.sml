
structure DictPairs
:
sig
  val getKey   : ''key * 'value -> ''key
  val getValue : ''key * 'value -> 'value
  val lookup   : (''key * 'value) list -> ''key -> 'value option
  val insert   : (''key * 'value) list -> ''key -> 'value -> (''key * 'value) list
  val entryExists : (''key * 'value) list -> ''key -> bool
  val predLookup : (''k * 'v) list-> (''k -> bool) -> ('v * (''k * 'v) list) option
  val filter : (''k * 'v) list -> (''k -> bool) -> (''k * 'v) list
end
=
struct
  fun getKey   (k, _) = k

  fun getValue (_, v) = v

  fun lookup dict k =
    let
        fun keyOfEntry t = (getKey t = k)
        val optionEntry = List.find keyOfEntry dict
    in
        Option.map getValue optionEntry
    end

  fun insert  dict  k v  =
    ((k,v) :: List.filter (not o (fn x => k = getKey x)) dict)

  fun entryExists dict k = Option.isSome (lookup dict k)

  fun predLookup [] _ = NONE
    | predLookup (entry::rest) p =
      if (p o getKey) entry
      then SOME (getValue entry, rest)
      else predLookup rest p

  fun filter dict p = List.filter (p o getKey) dict
end
