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

let rec merge_two ll a =
  match ll with
  | [] -> a
  | [h] -> h::a
  | h1::h2::t ->
    let m = merge h1 h2 [] in
    merge_two t (m::a)

let rec main ll =
  match ll with
  | [] -> []
  | [h] -> h
  | _ -> main (merge_two ll [])