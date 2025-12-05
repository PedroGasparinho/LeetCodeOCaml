open Printf
open TwoSumLib

let print_result a t f =
  let i, j = f a t in
  printf "%d + %d = %d, a[%d] = %d and a[%d] = %d\n" a.(i) a.(j) t i a.(i) j a.(j)

let test f =
  let t = [|
    ([|2; 7; 11; 15|], 9);
    ([|3; 2; 4|], 6);
    ([|3; 3|], 6)
  |] in
  for i = 0 to Array.length t - 1 do
    let (a, t) = t.(i) in
    print_result a t f
  done

let () =
  printf "Basic Two Sum\n";
  test Basic.two_sum;
  printf "\nOptimized Two Sum\n";
  test Optimized.two_sum