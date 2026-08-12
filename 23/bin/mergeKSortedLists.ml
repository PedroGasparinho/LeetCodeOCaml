open MergeKSortedListsLib
open UtilsLib

let test_n ll r = 
  let l = Basic.main ll in
  r = l

let test_1 = test_n [] []

let test_2 = test_n [[]] []

let test_3 = test_n [[1;4;5]; [1;3;4]; [2;6]] [1; 1; 2; 3; 4; 4; 5; 6]

let test_4 = test_n [[1;4;5]; [1;3;4]; [2;6]; [2;7;8]; [5;7;9]]
  [1; 1; 2; 2; 3; 4; 4; 5; 5; 6; 7; 7; 8; 9]

let t = [| test_1; test_2; test_3; test_4 |]

let () =
  Test.test t "Merge K Sorted Lists";