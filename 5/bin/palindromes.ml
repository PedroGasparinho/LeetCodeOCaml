open PalindromeLib
open TestLib

let test_n s l f =
  let r = f s in
  List.mem r l

let test_1 f =
  test_n "babad" ["aba"; "bab"] f

let test_2 f =
  test_n "cbbd" ["bb"] f

let test_3 f =
  test_n "racecar" ["racecar"] f

let t f = 
  [|test_1 f; test_2 f; test_3 f|] 

let () =
  Test.test (t Dynamic.longest) "Dynamic Programming Palindrome";
  print_endline "";
  Test.test (t Expand.longest) "Expading Outwards Palindrome";
  print_endline "";
  Test.test (t Manacher.longest) "Manacher's Algorithm Palindrome";