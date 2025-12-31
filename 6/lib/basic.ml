  open String

  let updateIndex i r rows b =
    let max = if rows > 2 then rows + (rows - 2) else rows in
    if r = 0 || r = rows - 1 then
      i := !i + max
    else if b then
      i := !i + max - 2*r
    else
      i := !i + 2*r

  let zigzag s rows =
    let s' = Array.make (length s) '?' and c = ref 0 in

    for r = 0 to rows - 1 do 
      let i = ref r in
      let b = ref true in
      while !i < length s do
        s'.(!c) <- get s !i;
        incr c;
        updateIndex i r rows !b;
        b := not !b
      done;
    done;
    init (length s) (fun x -> s'.(x))