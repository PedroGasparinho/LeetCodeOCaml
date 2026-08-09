open RmNthEndNodeLib
open TestLib

let test_n l n r = 
  let l' = Basic.main l n in
  r = l'

let test_1 = test_n [1; 2; 3; 4; 5] 1 [1; 2; 3; 4]

let test_2 = test_n [1; 2; 3; 4; 5] 2 [1; 2; 3; 5]

let test_3 = test_n [1; 2; 3; 4; 5] 3 [1; 2; 4; 5]

let test_4 = test_n [1; 2; 3; 4; 5] 4 [1; 3; 4; 5]

let test_5 = test_n [1; 2; 3; 4; 5] 5 [2; 3; 4; 5]

let test_6 = test_n [1] 1 []

let test_7 = test_n [1; 2] 1 [1]

let t = [| test_1; test_2; test_3; test_4; test_5; test_6; test_7 |]

let () =
  Test.test t "Remove Nth Node From End of List";