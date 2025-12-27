open PalindromeLib
open TestLib

let test_n s v =
  let r = Dynamic.longest s in
  r = v

let test_1 =
  test_n "babad" "aba"

let test_2 =
  test_n "cbbd" "bb"

let test_3 =
  test_n "racecar" "racecar"

let t = 
  [|test_1; test_2; test_3|] 

let () =
  Test.test t "Dynamic Programming Palindrome";