let rec water a l r =
  if l < 0 || r >= Array.length a || l >= r then 0
  else begin
    let h = min a.(l) a.(r) in
    let v =  h * (r - l) in
    let x = water a (l+1) r
    and y = water a l (r+1) in
    max (max x y) v
  end


let main a =
  water a 0 (Array.length a - 1)