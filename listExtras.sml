structure ListExtras =
struct
  open List
  fun exclude f        = filter (not o f)
  fun member list x    = Option.isSome (List.find (fn y => x = y) list)
  fun allTrue list     = all (fn x => x) list
  fun sameLength xs ys = (length xs = length ys)
  fun append xs ys     = xs @ ys

  fun remove _ [] = NONE
    | remove y (x::xs) =
      if x = y
      then SOME xs
      else Option.map (fn tl => x :: tl ) (remove y xs)

  (* true if for each member of the one list there is an equal member of the others *)
  fun eqMembers [] [] = true
    | eqMembers _  [] = false
    | eqMembers [] _  = false
    | eqMembers (x::xs) ys =
      case remove x ys
       of NONE     => false
        | SOME ys' => eqMembers xs ys'

  fun nub []      = []
    | nub (x::xs) = x::(nub o filter (fn y => y <> x)) xs

end

