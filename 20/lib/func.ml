let opening_par = function
  | ')' -> '('
  | '}' -> '{'
  | ']' -> '['
  | _ -> assert false

let rec handle_par s i q =
  if i >= String.length s then true
  else
    let c = String.get s i and
    f = handle_par s (i + 1) in
    match c with
    | '(' | '{' | '[' -> f (c::q)
    | ')' | '}' | ']' -> handle_closing_par c f q
    | _ -> assert false

and handle_closing_par c f = function
  | [] -> false
  | x::xs -> 
    if x = opening_par c then f xs
    else false

let main s =
  if String.length s mod 2 = 1 then false
  else handle_par s 0 []
