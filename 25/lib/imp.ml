let aux a = 
  let unique = ref 0 in
  for i = 1 to Array.length a - 1 do
    if a.(!unique) <> a.(i) then begin
      incr unique;
      a.(!unique) <- a.(i)
    end
  done;
  let u = !unique + 1 in
  (u, a)

let main a = 
  if Array.length a = 0 then (0, [||])
  else aux a