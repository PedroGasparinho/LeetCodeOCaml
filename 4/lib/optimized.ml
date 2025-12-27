let aux a1 a2 =
  let s1 = Array.length a1 and s2 = Array.length a2 in
  let l = ref 0 and r = ref s1 in
  let exception Break of float in
  try
    while !l <= !r do
      let m1 = (!l + !r) / 2 in
      let m2 = ((s1 + s2 + 1) / 2) - m1 in

      let maxLeft1 = if m1 = 0 then Int.min_int else a1.(m1 - 1)
      and minRight1 = if m1 = s1 then Int.max_int else a1.(m1)
      and maxLeft2 = if m2 = 0 then Int.min_int else a2.(m2 - 1)
      and minRight2 = if m2 = s2 then Int.max_int else a2.(m2) in

      if maxLeft1 <= minRight2 && maxLeft2 <= minRight1 then 
        begin
          if (s1 + s2) mod 2 = 0 then
            begin 
              let v = (max maxLeft1 maxLeft2 + min minRight1 minRight2) in
              raise (Break (Float.of_int v /. 2.))
            end
          else 
            begin
              let v = max maxLeft1 maxLeft2 in
              raise (Break (Float.of_int v))
            end
        end
      else if maxLeft1 > minRight2 then
        r := m1 - 1
      else
        l := m1 + 1
    done;
    raise Not_found
  with Break x -> x

let median a1 a2 =
  if Array.length a1 <= Array.length a2 then
    aux a1 a2
  else
    aux a2 a1