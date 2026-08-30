(* An interesting variation of the original problem, this program is guaranteed
   to remove one instance of the min and max values of the array, possibly with
   dups. Note: one can only remove from the head of tail of the array, as per
   the original problem. Thus, to deal with duplicates, which are not taken into
   consideration on the original problem, we must keep track of the leftmost and
   rightmost instance of each value (min/max). Then, we calculate the min of the
   4 possibilities:
    - leftmost min & mx,
    - rightmost mix & max,
    - leftmost min & rightmost max,
    - leftmost max & rightmost min. *)
let main a =
  let lmn = ref 0 and rmn = ref 0
  and lmx = ref 0 and rmx = ref 0 in
  for i = 1 to Array.length a - 1 do
    if (a.(i) = a.(!lmn)) then rmn := i;
    if (a.(i) = a.(!lmx)) then rmx := i;
    if (a.(i) < a.(!lmn)) then begin
      lmn := i;
      rmn := i
    end;
    if (a.(i) > a.(!lmx)) then begin
      lmx := i;
      rmx := i
    end;
  done;

  let l = max !lmn !lmx + 1
  and r = Array.length a - min !rmn !rmx
  and c1 = (!lmn + 1) + Array.length a - !rmx
  and c2 = (!lmx + 1) + Array.length a - !rmn in
  min (min l r) (min c1 c2)
