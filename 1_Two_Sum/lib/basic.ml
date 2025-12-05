let two_sum a t =
  let exception Break of int * int in
  try
    let l = Array.length a - 1 in
    for i = 0 to l-1 do
      for j = 1 to l do
        if a.(i) + a.(j) = t then raise (Break (i, j))
      done;
    done;
    raise Not_found
  with Break (i, j) -> (i, j)