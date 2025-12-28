open String

let rec expand s i j = 
  if i >= 0 && j < length s && get s i = get s j then
    expand s (i-1) (j+1)
  else
    String.sub s (i+1) (j-i-1)

let tryUpdate best cand =
  if length cand > length !best then 
    best := cand

let longest s =
  let best = ref "" in
  for i = 0 to length s - 1 do
    let odd = expand s i i in
    tryUpdate best odd;
    let even = expand s i (i+1) in
    tryUpdate best even;
  done;
  !best