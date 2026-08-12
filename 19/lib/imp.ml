open UtilsLib.Llist

exception Break of int list

let main l n =
  try
    let l' = from_list l in
    let head = force_some (l'.head) in
   
    if head.next = None then raise (Break []);
  
    let s = ref head and f = ref head in
    for _ = 0 to (n-1) do
      if !f.next = None then raise (Break (to_list {head = !s.next}));
      force_next f
    done;

    while (!f.next <> None) do
      safe_next s;
      safe_next f
    done;
    !s.next <- get_two_ahead s;
    to_list l'
  with Break l -> l