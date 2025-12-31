open RevIntLib
open TestLib

let test_n x v =
  let r = Basic.revInt x in
  r = v

let test_1 =
  test_n 123l 321l

let test_2 =
  test_n (-123l) (-321l)

let test_3 =
  test_n 120l 21l

let t = 
  [|test_1; test_2; test_3|] 

let () =
  Test.test t "Reverse Int32";