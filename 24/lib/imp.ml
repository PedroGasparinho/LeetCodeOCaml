open UtilsLib.Llist

let aux l = 
  let s = ref None
  and p = ref (force_some l.head) in
  while (!p.next <> None) do
    let n = force_some !p.next in
    !p.next <- n.next;
    n.next <- Some (!p);
    if !s = None then begin l.head <- Some n end
    else begin 
      let s' = force_some !s in s'.next <- Some n
    end;
    s:= Some !p;
    if !p.next <> None then force_next p;
  done;
  to_list l

let main = function
  | [] -> []
  | [x] -> [x]
  | l -> aux (from_list l)