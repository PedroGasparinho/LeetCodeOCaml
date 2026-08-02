open PaliNumLib
open TestLib

let test_n n = Basic.main n

let test_1 = test_n 121

let test_2 = not (test_n (-12))

let test_3 = not (test_n 10)

let test_4 = test_n 0

let test_5 = not (test_n 100)

let test_6 = test_n 1221

let test_7 = not (test_n 1231)

let test_8 = not (test_n 1321)

let test_9 = not (test_n 1234)

let test_10 = not (test_n 4321)

let t = 
  [|
    test_1; test_2; test_3; test_4; test_5;
    test_6; test_7; test_8; test_9; test_10
  |]

let () =
  Test.test t "Palidrome Numbers";