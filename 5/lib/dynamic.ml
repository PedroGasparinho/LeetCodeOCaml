open String

let longest s =
  let l = length s in
  let m = Array.init l (fun x -> Array.init l (fun y -> x = y)) in
  let best = ref (0, 0) in

  for i = 0 to l-2 do
    if get s i = get s (i+1) then begin
      m.(i).(i+1) <- true;
      best := (i, i+1)
    end    
  done;

  for k = 2 to l-1 do
    for i = 0 to l - k - 1 do
      let j = i + k in
      if get s i = get s j && m.(i+1).(j-1) then begin
        m.(i).(j) <- true;
        best := (i, j)
      end
    done;
  done;

  let pos = fst !best in
  let size = snd !best - pos + 1 in
  String.sub s pos size