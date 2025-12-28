open String

let canIncrement i a l p =
  let newRight = i + 1 + a.(i) and newLeft = i - 1 - a.(i) in
  newRight < l &&
  newLeft >= 0 &&
  get p (newRight) = get p (newLeft)


let longest s =
  let l = 1 + 2 * length s in
  let p = init l (fun c -> if c mod 2 = 0 then '#' else get s (c/2)) in
  let a = Array.make l 0 in
  let center = ref 0 and radius = ref 0 in

  for i = 0 to l-1 do
    let mirror = 2 * !center - i in

    if i < !radius then begin 
      a.(i) <- min (!radius - i) a.(mirror) 
    end;

    while canIncrement i a l p do
      a.(i) <- a.(i) + 1
    done;

    if i + a.(i) > !radius then begin
      center := i;
      radius := i + a.(i)
    end

  done;

  let maxLength = ref 0 and centerIndex = ref 0 in

  for i = 0 to l - 1 do
    if a.(i) > !maxLength then begin
      maxLength := a.(i);
      centerIndex := i
    end
  done;

  let startIndex = (!centerIndex - !maxLength) / 2 in
  try
  String.sub s startIndex !maxLength
  with _ -> 
    begin 
      print_int startIndex; 
      print_string " ";
      print_int (startIndex + !maxLength);
      print_endline (" " ^ s);
      ""
    end