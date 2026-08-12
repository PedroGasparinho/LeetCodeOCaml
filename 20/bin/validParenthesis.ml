open ValidParenthesisLib
open UtilsLib

let test_n f s = f s

let test_1 f = test_n f "()"

let test_2 f = test_n f "()[]{}"

let test_3 f = not (test_n f "(]")

let test_4 f = test_n f "([])"

let test_5 f = not (test_n f "([)]")

let test_6 f = not (
  test_n f "(" || test_n f ")" ||
  test_n f "{" || test_n f "}" ||
  test_n f "[" || test_n f "]"
)

let t f = [| test_1 f; test_2 f; test_3 f; test_4 f; test_5 f; test_6 f |]

let () =
  Test.test (t Basic.main) "Valid Parenthesis (Imperative)";
  print_endline "\n";
  Test.test (t Func.main) "Valid Parenthesis (Functional)"