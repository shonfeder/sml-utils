structure ListPlus =
struct
  open List

  (* For consistency with other collection types *)
  val empty = []
  fun add x ls = x::ls

  (* shadows default parital list hd & tl with total equivalents *)
  fun hd (x::_) = SOME x
    | hd []     = NONE
  fun tl (_::xs) = SOME xs
    | tl []      = NONE

  fun repeat n f       = tabulate (n, fn _ => f)
  fun exclude f        = filter (not o f)

  (* member check for quality types *)
  fun memberEq x list  = List.exists (fn y => x = y) list

  (* member check on ordered types *)
  fun member compare x list = List.exists (fn y => compare (x,y) = EQUAL) list

  (* remove first matching element, for eqtypes *)
  fun removeEq _ [] = NONE
    | removeEq y (x::xs) =
      if x = y
      then SOME xs
      else Option.map (fn tl => x :: tl ) (removeEq y xs)
  (* remove first equal element, by orderable types *)

  fun remove _ _ [] = []
    | remove compare y (x::xs) =
      case compare (y,x)
       of EQUAL => xs
        | _     => x :: (remove compare y xs)

  fun allTrue list     = all (fn x => x) list
  fun sameLength xs ys = (length xs = length ys)
  fun append xs ys     = xs @ ys
  fun split n ls       = (take (ls,n), drop (ls,n))


  (* true if for each member of the one list there is an equal member of the others *)
  fun eqMembers [] [] = true
    | eqMembers _  [] = false
    | eqMembers [] _  = false
    | eqMembers (x::xs) ys =
      case removeEq x ys
       of NONE     => false
        | SOME ys' => eqMembers xs ys'

  fun nub []      = []
    | nub (x::xs) = x::(nub o filter (fn y => y <> x)) xs

  fun toString toStr list =
    let
        fun parse []  = ""
          | parse [x] = toStr x
          | parse (x::xs) = toStr x ^ ", " ^ parse xs
    in
        "["^ parse list ^ "]"
    end

end (* ListPlus *)
