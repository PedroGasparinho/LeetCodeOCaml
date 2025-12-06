open TwoSumLib
open TestLib
open Printf

let test_n a t f =
  let (i, j) = f a t in
  a.(i) + a.(j) = t

let test_1 f =
  test_n [|2; 7; 11; 15|] 9 f

let test_2 f =
  test_n [|3; 2; 4|] 6 f

let test_3 f =
  test_n [|3; 3|] 6 f

let t f = 
  [|test_1 f; test_2 f; test_3 f|] 

let () =
  Test.test (t Basic.two_sum) "Two Sum (Basic)";
  printf "\n";
  Test.test (t Optimized.two_sum) "Two Sum (Optimized)"