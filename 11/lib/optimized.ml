let rec water a l r m =
  if l >= r then m
  else begin
    let h = min a.(l) a.(r) in
    let v = max (h * (r - l)) m in
    if a.(l) > a.(r) then
      water a l (r-1) v
    else
      water a (l+1) r v
  end

let main a =
  water a 0 (Array.length a - 1) 0