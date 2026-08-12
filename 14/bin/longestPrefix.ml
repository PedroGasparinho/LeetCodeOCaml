open LongestPrefixLib
open UtilsLib

let test_n a r = 
  let p = Basic.main a in
  String.equal p r

let test_1 = test_n [|"flower"; "flow"; "flight"|] "fl"

let test_2 = test_n [|"flower"; "flow"; "fight"|] "f"

let test_3 = test_n [|"flower"; "flow"; "florida"|] "flo"

let test_4 = test_n [|"dog"; "racecar"; "car"|] ""

let t = [| test_1; test_2; test_3; test_4 |]

let () =
  Test.test t "Longest Common Prefix";