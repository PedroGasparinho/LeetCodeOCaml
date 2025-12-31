open Printf
  
let update res b =
  if b then 
    res := (fst !res + 1, snd !res)
  else 
    res := (fst !res, snd !res + 1)

let print_case b i =
  let s = if b then "Passed" else "Failed" in
  printf " - %s test #%d\n" s i

let print_final (p, f) =
  let t = p+f in
  printf "Passed: %d/%d, Failed: %d/%d\n" p t f t

let test t n =
  let l = Array.length t in
  printf "%s:\n" n;
  let res = ref (0, 0) in
  for i = 0 to l - 1 do
    let r = t.(i) in
    update res r;
    print_case r (i+1)
  done;
  print_final !res