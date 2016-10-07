(* Extension of the ArraySlice structure, to let it work independent of the Array structure *)

signature ARRAY_SLICE_PLUS =
sig
    include ARRAY_SLICE
    val eqItems  : ''a slice * ''b slice -> bool
    val fromList : 'a slice -> 'a list
    val toList   : 'a list  -> 'a slice
    val join     : 'a slice * 'a slice -> 'a slice
    val array    : 'a slice -> 'a array
end

structure ArraySlicePlus =
struct

    open ArraySlice

    exception Slice

    fun eqItems (arrA,arrB) =
      case (ArraySlice.getItem arrA, ArraySlice.getItem arrB)
       of (NONE, NONE) => true
        | ((NONE, _) | (_, NONE)) => false
        | (SOME (a, arrA'), SOME (b, arrB')) => a = b andalso eqItems (arrA', arrB')

    fun fromList list = (ArraySlice.full o Array.fromList) list
    fun toList arr = case ArraySlice.getItem arr
                      of NONE => []
                       | SOME (a, arr') => a :: toList arr'

    fun baseArray arr = #1 (base arr)
    fun start arr     = #2 (base arr)
    fun stop arr      = (start arr) + (#3 (base arr))


    fun join (arrA,arrB) =
      let
          val baseArr = baseArray arrA
      in
          if
              baseArray arrB = baseArr
              andalso
              stop arrA = start arrB
          then
              slice (baseArr, start arrA, SOME (stop arrB))
          else
              raise Slice
      end

    fun array sl =
      Array.tabulate (length sl, fn i => sub (sl, i))


      end (* ArraySlicePlus *)

structure Tests =
struct
    structure Slice = ArraySlicePlus
    val arr = Slice.fromList [6,3,4,5,2,1]
    val front = Slice.subslice (arr, 0, SOME 3)
    val midf  = Slice.subslice (arr, 1, SOME 2)
    val midb  = Slice.subslice (arr, 3, SOME 2)
    val back  = Slice.subslice (arr, 3, NONE)
end
