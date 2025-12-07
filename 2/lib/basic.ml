let addTwoNumbers l1 l2 =
  let rec aux l1 l2 d = 
    match l1, l2 with
    | [], [] -> 
      if d > 0 then [d] else []
    | h::t, [] | [], h::t -> 
      let sum = h + d in
      (sum mod 10)::aux t [] (sum / 10)
    | h1::t1, h2::t2 -> 
      let sum = h1 + h2 + d in
      (sum mod 10)::aux t1 t2 (sum / 10)
  in aux l1 l2 0
  