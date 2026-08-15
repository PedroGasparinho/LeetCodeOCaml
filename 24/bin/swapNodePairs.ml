open SwapNodePairsLib
open UtilsLib

let test_n l r f = 
  let l' = f l in
  r = l'

let test_1 f = test_n [] [] f

let test_2 f = test_n [1] [1] f

let test_3 f = test_n [101] [101] f

let test_4 f = test_n [1; 2] [2; 1] f

let test_5 f = test_n [1; 2; 3] [2; 1; 3] f

let test_6 f = test_n [1; 2; 3; 4] [2; 1; 4; 3] f

let test_7 f = test_n [1; 2; 3; 4; 5; 6; 7; 8] [2; 1; 4; 3; 6; 5; 8; 7] f

let t f = [|
  test_1 f; test_2 f; test_3 f; test_4 f; test_5 f; test_6 f; test_7 f
|]

let () =
  Test.test (t Basic.main) "Reverse every 2 nodes of List (Func)";
  print_endline "";
  Test.test (t Imp.main) "Reverse every 2 nodes of List (Imp)"