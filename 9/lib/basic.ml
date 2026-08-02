let rec halfRev x r = 
  if x <= r then (x, r)
  else halfRev (x / 10) (r * 10 + x mod 10)

let main n =
  if n < 0 then false
  else if n mod 10 = 0 && n <> 0 then false
  else 
    let x, r = halfRev n 0 in
    x = r || x = r / 10