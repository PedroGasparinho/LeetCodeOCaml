let rec merge l1 l2 a =
  match l1, l2 with
  | [], [] -> List.rev a
  | [], _ -> List.rev_append a l2
  | _, [] -> List.rev_append a l1
  | x::xs, y::ys ->
    if x <= y then
      merge xs l2 (x::a)
    else
      merge l1 ys (y::a)

let main l1 l2 =
  merge l1 l2 []
