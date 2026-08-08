let main a =
  let d = ref true
  and p = ref 0
  and l = Array.length a - 1 in
  while !d do
    let c = String.get a.(0) !p in
    for i = 1 to l do 
      if c <> String.get a.(i) (!p) then d := false
    done;
    incr p
  done;
  String.sub a.(0) 0 (!p - 1)