open Printf
open TwoSumLib

let check a t f =
  let (i, j) = f a t in
  assert (a.(i) + a.(j) = t)

let check_all f = 
  let tests = [|
    ([|2; 7; 11; 15|], 9);
    ([|3; 2; 4|], 6);
    ([|3; 3|], 6)
  |] in

  let l = Array.length tests in
  for i = 0 to l - 1 do
    let (a, t) = tests.(i) in
    check a t f;
    printf " - Passed %d/%d\n" (i+1) l
  done


let () =
  printf "Testing Two Sum (Basic):\n";
  check_all Basic.two_sum;
  printf "Testing Two Sum (Optimized):\n";
  check_all Optimized.two_sum