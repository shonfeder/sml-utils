(* Extension of the ArraySlice structure, to let it work independent of the Array structure *)

(* signature ARRAY_SLICE_PLUS = *)
(* sig *)
(*     include ARRAY_SLICE *)
(*     val empty    : 'a slice -> 'a slice *)
(*     val eqItems  : ''a slice * ''b slice -> bool *)
(*     val fromList : 'a slice -> 'a list *)
(*     val toList   : 'a list  -> 'a slice *)
(*     val join     : 'a slice * 'a slice -> 'a slice *)
(*     val array    : 'a slice -> 'a array *)
(*     val splitAt  : ('a slice * int) -> ('a slice * 'a slice) *)
(*     val splitAround : 'a slice' *)
(*     val subOpt   : 'a slice -> int -> 'a slice *)
(*     val swap     : 'a slice -> (int * int) -> 'a slice *)
(* end *)

structure ArraySlicePlus =
struct

    infix 3 />    fun x /> f = f x

    open ArraySlice
    exception Slice

    fun empty arr = subslice (arr, 0, SOME 0)

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
    fun final arr     = (stop arr) - 1

    fun contiguous (arrA,arrB) =
        baseArray arrB = baseArray arrA
        andalso
        stop arrA = start arrB

    fun join (arrA,arrB) =
      if contiguous (arrA,arrB)
      then SOME (slice (baseArray arrA, start arrA, SOME (stop arrB)))
      else NONE

    fun extend n arr = let
        val baseArr = baseArray arr
        fun withinBounds arr = (stop arr) + n <= Array.length baseArr
        fun extend' arr = slice (baseArr, start arr, SOME ((stop arr)+n))
    in
        Option.compose (extend', Option.filter withinBounds) arr
    end

    fun annex (a,b) =
      if contiguous (a,b)
      then case getItem b
            of NONE => NONE
             | SOME (toAnnex, b') =>
           case (extend 1 a)
            of NONE => NONE
             | SOME a' => SOME (a', b')
      else NONE

    fun array sl =
      Array.tabulate (length sl, fn i => sub (sl, i))

    fun drop n arr = subslice (arr, n, NONE)

    fun splitAt n arr = let
        fun withinIndex arr = (length arr+1) > n
        fun front arr = subslice (arr, 0, SOME n)
        fun back  arr = subslice (arr, n, NONE)
        fun splitInTwo arr = (front arr, back arr)
        val arrOpt = Option.filter withinIndex arr
    in
        Option.map splitInTwo arrOpt
    end

    fun splitAround n arr = let
        fun splitAroundN (front, back) = (Fn.id front, drop 1 back)
    in
        (Option.map splitAroundN o splitAt n) arr
    end


    fun subOpt arr i =
      if i >= length arr
      then NONE
      else SOME (sub (arr, i))

    fun swap arr (i,j)  = let
        val ith = sub (arr, i)
        val jth = sub (arr, j)
        val ()  = update(arr, i, jth)
        val ()  = update(arr, j, ith)
    in
        arr
    end

    fun swapEnds arr = swap arr (start arr, final arr)


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
