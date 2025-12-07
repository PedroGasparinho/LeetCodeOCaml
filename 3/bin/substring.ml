open SubstringLib
open TestLib

let test_n s v =
  let r = Basic.longestSubstring s in
  r = v

let test_1 =
  test_n "abcabcbb" 3

let test_2 =
  test_n "bbbbb" 1

let test_3 =
  test_n "pwwkew" 3

let test_4 =
  test_n "abcadcbb" 4

let t = 
  [|test_1; test_2; test_3; test_4|] 

let () =
  Test.test (t ) "Longest Substring w/o duplicates";