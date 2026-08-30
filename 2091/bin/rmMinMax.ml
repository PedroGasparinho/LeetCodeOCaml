open RmMinMaxLib
open UtilsLib

let test_n a v =
  let r = Basic.main a in
  r = v

let test_1 =
  test_n [|101|] 1

let test_2 =
  test_n [|2; 10; 7; 5; 4; 1; 8; 6|] 5

let test_3 =
  test_n [|0; -4; 19; 1; 8; -2; -3; 5|] 3

let t = 
  [|test_1; test_2; test_3|]

let () =
  Test.test t "Remove Mix Max From Array";