open WaterContainerLib
open UtilsLib

let test_n a f = f a

let test_1 f = test_n [| 1; 1 |] f = 1

let test_2 f = test_n [|1;8;6;2;5;4;8;3;7|] f = 49

let t f = [| test_1 f; test_2 f|]

let () = 
  Test.test (t Basic.main) "Water Container Area (Basic)";
  print_endline "";
  Test.test (t Optimized.main) "Water Container Area (Optimized)"