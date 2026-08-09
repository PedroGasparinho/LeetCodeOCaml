exception Break of bool

let opening_par = function
  | ')' -> '('
  | '}' -> '{'
  | ']' -> '['
  | _ -> assert false

let handle_closing_par c q =
  match !q with
  | [] -> raise (Break false)
  | x::xs -> 
    if x = opening_par c then q := xs
    else raise (Break false)

let handle_par c q =
  match c with
  | '(' | '{' | '[' -> q := (c :: !q)
  | ')' | '}' | ']' -> handle_closing_par c q
  | _ -> assert false

let main s = 
  try
    if String.length s mod 2 = 1 then raise (Break false);
    let q = ref [] in
    for i = 0 to String.length s - 1 do
      handle_par (String.get s i) q
    done;
    true
  with Break b -> b