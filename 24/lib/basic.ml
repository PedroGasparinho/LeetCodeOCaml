let rec swapNodePairs a = function
  | [] -> List.rev a
  | [h] -> List.rev (h::a)
  | h1::h2::l -> swapNodePairs (h1::h2::a) l

let main l = swapNodePairs [] l