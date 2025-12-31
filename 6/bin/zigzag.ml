open ZigzagLib
open TestLib

let test_n s rows v =
  let r = Basic.zigzag s rows in
  r = v

let p = "paypalishiring"

let test_1 =
  test_n p 3 "pahnaplsiigyir"

let test_2 =
  test_n p 4 "pinalsigyahrpi"

let test_3 =
  test_n p 5 "phasiyirpligan"

let test_4 =
  test_n "a" 1 "a"

let test_5 =
  test_n "abcd" 1 "abcd"

let test_6 =
  test_n "abcd" 2 "acbd"

let test_7 =
  test_n "abcd" 3 "abdc"

let test_8 =
  test_n "abcd" 5 "abcd"

let t = 
  [|test_1; test_2; test_3; test_4; test_5; test_6; test_7; test_8|] 

let () =
  Test.test t "Longest Substring w/o duplicates";