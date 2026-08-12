let rec rmNthEndNode i a = function
  | x::xs -> 
    if i = 0 then
      List.rev_append a xs 
    else 
      rmNthEndNode (i-1) (x::a) xs
  | [] -> []

let main l n =
  rmNthEndNode (List.length l - n) [] l

(*
The fast and slow pointers approach is not ideal in a fully functional
environment, since we don't have pointers. Instead, we would have a slow
and a fast list, and, thus, O(n) space.
*)