open MedianLib
open TestLib

let test_n a1 a2 v f =
  let r = f a1 a2 in
  r = v

let test_1 f =
  test_n [|1; 3|] [|2|] 2. f

let test_2 f =
  test_n [|1; 2|] [|3; 4|] 2.5 f

let test_3 f =
  test_n [|0; 2; 5; 6; 8|] [|1; 2; 3; 4; 7; 9|] 4. f

let test_4 f =
  test_n [|0; 1; 2|] [|0; 0; 2; 2|] 1. f

let test_5 f =
  test_n [|0; 7; 8|] [|2; 2; 2|] 2. f

let test_6 f =
  test_n [|1; 2; 3; 4; 9|] [|0; 5; 6; 7; 8|] 4.5 f

let t f = 
  [|test_1 f; test_2 f; test_3 f; test_4 f; test_5 f; test_6 f|] 

let () =
  Test.test (t Basic.median) "Basic Median";
  print_endline "";
  Test.test (t Optimized.median) "Optimized Median";