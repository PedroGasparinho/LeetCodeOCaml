let two_sum a t =
  let exception Break of int * int in
  try 
    let l = Array.length a in
    let h = Hashtbl.create (l) in
    Hashtbl.add h (t - a.(0)) 0;
    for j = 1 to l do
      match Hashtbl.find_opt h a.(j) with
      | None -> Hashtbl.add h (t - a.(j)) j
      | Some i -> raise (Break (i, j))
    done;
    raise Not_found
  with Break (i, j) -> (i, j)