open AddTwoNumbersLib
open TestLib

let test_n l1 l2 e =
  List.equal (=) e (Basic.addTwoNumbers l1 l2)

let test_1 =
  test_n [2; 4; 3] [5; 6; 4] [7; 0; 8]

let test_2 =
  test_n [0] [0] [1]

let test_3 =
  test_n [9; 9; 9; 9; 9; 9; 9] [9; 9; 9; 9] [8; 9; 9; 9; 0; 0; 0; 1]

let t = 
  [|test_1; test_2; test_3|] 

let () = (*print_int (Array.length t)*)
  Test.test t "Add Two Numbers"