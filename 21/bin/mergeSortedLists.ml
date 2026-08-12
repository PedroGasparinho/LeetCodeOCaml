open MergeSortedListsLib
open UtilsLib

let test_n l1 l2 r = 
  let l' = Basic.main l1 l2 in
  r = l'

let test_1 = test_n [] [] []

let test_2 = test_n [] [9] [9]

let test_3 = test_n [-1] [1] [-1; 1]

let test_4 = test_n [1; 2; 4] [1; 3; 4] [1; 1; 2; 3; 4; 4]

let test_5 = test_n [1; 2; 4; 6] [1; 3; 4; 5] [1; 1; 2; 3; 4; 4; 5; 6]

let t = [| test_1; test_2; test_3; test_4; test_5 |]

let () =
  Test.test t "Merge Two Sorted Lists";