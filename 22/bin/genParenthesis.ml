open GenParenthesisLib
open UtilsLib

let test_n n r = 
  let l = Basic.main n in
  r = l

let test_1 = test_n 1 ["()"]

let test_2 = test_n 2 ["(())"; "()()"]

let test_3 = test_n 3 ["((()))"; "(()())"; "(())()"; "()(())"; "()()()"]

let test_4 = test_n 4 [
  "(((())))"; "((()()))"; "((())())"; "((()))()"; "(()(()))"; "(()()())";
  "(()())()"; "(())(())"; "(())()()"; "()((()))"; "()(()())"; "()(())()";
  "()()(())"; "()()()()"
]

let t = [| test_1; test_2; test_3; test_4 |]

let () =
  Test.test t "Merge Two Sorted Lists";