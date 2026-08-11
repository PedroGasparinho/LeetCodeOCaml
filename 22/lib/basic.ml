let main n =
  let rec gen_par op cl s = 
    if op = 0 && cl = 0 then [s]
    else begin
      let l1 = open_par op cl s
      and l2 = close_par op cl s
      in l1 @ l2
    end
  and open_par op cl s =
    if op > 0 then gen_par (op - 1) cl (s ^ "(")
    else []
  and close_par op cl s =
    if cl > op then gen_par op (cl - 1) (s ^ ")")
    else []
  in gen_par n n ""