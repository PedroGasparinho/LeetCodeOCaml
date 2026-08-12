open RmNthEndNodeLib
open UtilsLib

let test_n l n r f = 
  let l' = f l n in
  r = l'

let test_1 f = test_n [1; 2; 3; 4; 5] 1 [1; 2; 3; 4] f

let test_2 f = test_n [1; 2; 3; 4; 5] 2 [1; 2; 3; 5] f

let test_3 f = test_n [1; 2; 3; 4; 5] 3 [1; 2; 4; 5] f

let test_4 f = test_n [1; 2; 3; 4; 5] 4 [1; 3; 4; 5] f

let test_5 f = test_n [1; 2; 3; 4; 5] 5 [2; 3; 4; 5] f

let test_6 f = test_n [1] 1 [] f

let test_7 f = test_n [1; 2] 1 [1] f

let t f = [|
  test_1 f; test_2 f; test_3 f; test_4 f; test_5 f; test_6 f; test_7 f
|]

let () =
  Test.test (t Basic.main) "Remove Nth Node From End of List (Func)";
  print_endline "";
  Test.test (t Imp.main) "Remove Nth Node From End of List (Func)"