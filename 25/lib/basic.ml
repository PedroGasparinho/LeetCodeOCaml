let main l =
  let rec aux a = function
    | [] -> List.rev a
    | [h] -> List.rev (h::a)
    | h1::h2::t -> 
      if h1 = h2 then aux a (h2::t)
      else aux (h1::a) (h2::t)
  in begin
    let r = aux [] l in
    (List.length r, r)
  end