let auxOver = Int32.div Int32.max_int 10l 
and auxUnder = Int32.div Int32.min_int 10l

let isOverflow x p =
  x > auxOver ||
  (x = auxOver && p > 7l) ||
  x < auxUnder ||
  (x = auxUnder && p < -8l)

let revInt (x: int32) =
  let x' = ref x in
  let rev = ref 0l in
  let exception Overflow in
  try
    while !x' <> 0l do
      let p = Int32.rem !x' 10l in
      x' := Int32.div !x' 10l;
      if isOverflow !rev p then 
        raise Overflow 
      else 
        rev := Int32.add (Int32.mul !rev 10l) p
    done;
    !rev
  with Overflow -> 0l