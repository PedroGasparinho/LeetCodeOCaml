open MaxBalloonsLib
open UtilsLib

let test_n s v =
  let r = Basic.main s in
  r = v

let test_1 =
  test_n "nlaebolko" 1

let test_2 =
  test_n "loonbalxballpoon" 2

let test_3 =
  test_n "leetcode" 0

let test_4 =
  test_n "balloonn" 1

let test_5 =
  test_n "baloon" 0

let test_6 =
  test_n "ballon" 0

let t = 
  [|test_1; test_2; test_3; test_4; test_5; test_6|]

let () =
  Test.test t "Maximum Number of Balloons";