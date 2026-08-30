let main a =
  let mn = ref 0 and mx = ref 0 in
  for i = 1 to Array.length a - 1 do
    if (a.(i) < a.(!mn)) then mn := i;
    if (a.(i) > a.(!mx)) then mx := i
  done;
  let aux = !mn in
  mn := min !mn !mx;
  mx := max aux !mx;
  let l = !mx + 1
  and r = Array.length a - !mn
  and c = (!mn + 1) + Array.length a - !mx in
  min c (min l r)
