let isInBounds a i =
  0 <= i && i < Array.length a

let update a p last curr =
  last := !curr;
  curr := Some a.(!p);
  incr p

let handleUpdate a1 p1 a2 p2 last curr =
  let b1 = isInBounds a1 !p1 and b2 = isInBounds a2 !p2 in
  if b1 && b2 then 
    begin
      if a1.(!p1) < a2.(!p2) then update a1 p1 last curr 
      else update a2 p2 last curr 
    end 
  else if b1 then update a1 p1 last curr 
  else if b2 then update a2 p2 last curr 
  else assert false

let handleResult s curr last =
  let x = Option.get curr in
  if s mod 2 = 0 then begin
    let y = Option.get last in 
    let r = x+y in 
    (Float.of_int r) /. 2.
  end else Float.of_int x

let median a1 a2 =  
  let s = Array.length a1 + Array.length a2 in
  let l = s / 2 in
  let p1 = ref 0 and p2 = ref 0 in
  let last = ref None and curr = ref None in
  for _ = 0 to l do
    handleUpdate a1 p1 a2 p2 last curr
  done;
  handleResult s !curr !last
