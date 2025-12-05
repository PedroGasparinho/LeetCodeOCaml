open Printf

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

let get_result a t =
  let i, j = two_sum a t in
  printf "%d + %d = %d, a[%d] = %d and a[%d] = %d\n" a.(i) a.(j) t i a.(i) j a.(j)

let () =
  get_result [|2; 7; 11; 15|] 9;
  get_result [|3; 2; 4|] 6;
  get_result [|3; 3|] 6;