(* Extension of the ArraySlice structure

  Enables use of ArraySlices without having to bounce
  back into the Array module.

  Also provides a number of helpful functions facilitating
  declarative use of array slices. *)

signature ARRAY_SLICE_PLUS =
sig
    include ARRAY_SLICE
    val empty    : 'a slice -> 'a slice
    val eqItems  : ''a slice * ''a slice -> bool
    val contiguous : 'a slice * 'a slice -> bool
    (** ~contiguous a b~ is true when ~a~ and ~b~ share the same base
        array and the last element of ~a~ immediately precedes the first
        element of ~b~ on the base array*)

    (** Conversions *)
    val fromList : 'a list -> 'a slice
    val toList   : 'a slice -> 'a list
    val toArray  : 'a slice -> 'a array

    val baseArray : 'a slice -> 'a array
    (** ~baseArray arr~ is the base array on which the slice ~arr~ is defined *)
    val start : 'a slice -> int
    val stop  : 'a slice -> int
    (** ~start a~ and ~stop a~ are the index of the first and last elements of
        the slice ~a~ on the base array, respectively *)

    val join : 'a slice * 'a slice -> 'a slice option
    (** ~join a b~ is ~SOME~ slice where ~a~ is appended to ~b~ if ~a~ and ~b~
        are contiguous, or else ~NONE~. *)
    val extend : int -> 'a slice -> 'a slice option
    (** ~extend n b~ is ~SOME~ slice comprised of ~a~ extended the next ~n~
        elements  of the base array if there are ~n~ elements available on the
        base array; otherwise, it is ~NONE~. *)
    val annex : 'a slice * 'a slice -> ('a slice * 'a slice) option
    (** If ~contiguous (a,b)~ then ~annex (a, b)~ is ~SOME~ pair of slices
        ~(c,d)~ where ~c~ is ~a~ with the first member of ~b~ appended, and
        ~d~ is ~b~ with the first member removed. Otherwise, it is ~NONE~.

     E.g.,

        - Pair.map toArray arrs;
        val it = ([|1,2|],[|3,4|]) : int array * int array
        - (Option.map (Pair.map toArray) (annex arrs));
        val it = SOME ([|1,2,3|],[|4|]) : (int array * int array) option
     *)

    val splitAt     : int -> 'a slice -> ('a slice * 'a slice) option
    val splitAround : int -> 'a slice -> ('a slice * 'a slice) option

    val subOpt : 'a slice -> int -> 'a option
    (** A total alternative to the partial ArraySlice.sub *)
    val drop : int -> 'a slice -> 'a slice

    val swap : 'a slice -> (int * int) -> 'a slice
    (** swap the elements at the given indexes *)
    val swapEnds : 'a slice -> 'a slice
    (** swap the first and last elements of the given slice *)
end

structure ArraySlicePlus : ARRAY_SLICE_PLUS =
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

    fun join (arrA, arrB) =
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

    fun toArray sl =
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
