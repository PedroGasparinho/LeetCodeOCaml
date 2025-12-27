open MedianLib
open TestLib

let test_n a1 a2 v =
  let r = Basic.median a1 a2 in
  r = v

let test_1 =
  test_n [|1; 3|] [|2|] 2.

let test_2 =
  test_n [|1; 2|] [|3; 4|] 2.5 

let test_3 =
  test_n [|0; 2; 5; 6; 8|] [|1; 2; 3; 4; 7; 9|] 4.

let test_4 =
  test_n [|0; 1; 2|] [|0; 0; 2; 2|] 1.

let test_5 =
  test_n [|0; 7; 8|] [|2; 2; 2|] 2.

let test_6 =
  test_n [|1; 2; 3; 4; 9|] [|0; 5; 6; 7; 8|] 4.5

let t = 
  [|test_1; test_2; test_3; test_4; test_5; test_6|] 

let () =
  Test.test (t ) "Basic Median";