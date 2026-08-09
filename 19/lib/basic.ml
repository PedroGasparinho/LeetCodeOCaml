let rec rmNthEndNode i a = function
  | x::xs -> 
    if i = 0 then
      List.rev_append a xs 
    else 
      rmNthEndNode (i-1) (x::a) xs
  | [] -> []

let main l n =
  rmNthEndNode (List.length l - n) [] l