open RemDupsSortedLib
open UtilsLib.Test

let test_na l (s, r) = 
  let (s', r') = Basic.main l in
  r = r' && s = s' && s' = List.length r

let test_1a = test_na [] (0, [])

let test_2a = test_na [1] (1, [1])

let test_3a = test_na [1; 2; 3] (3, [1; 2; 3])

let test_4a = test_na [1; 2; 2; 3; 3; 3] (3, [1; 2; 3])

let test_5a = test_na [1; 1; 1; 2; 2; 3] (3, [1; 2; 3])

let test_6a = test_na [1; 1; 1; 1; 2; 3] (3, [1; 2; 3])

let ta = [| test_1a; test_2a; test_3a; test_4a; test_5a; test_6a |]

let test_nb a (s, r) =
  let (s', r') = Imp.main a in
  assert (s = s');
  for i = 0 to s' - 1 do
    assert (r.(i) = r'.(i))
  done;
  true

let test_1b = test_nb [||] (0, [||])

let test_2b = test_nb [|1|] (1, [|1|])

let test_3b = test_nb [|1; 2; 3|] (3, [|1; 2; 3|])

let test_4b = test_nb [|1; 2; 2; 3; 3; 3|] (3, [|1; 2; 3|])

let test_5b = test_nb [|1; 1; 1; 2; 2; 3|] (3, [|1; 2; 3|])

let test_6b = test_nb [|1; 1; 1; 1; 2; 3|] (3, [|1; 2; 3|])

let tb = [| test_1b; test_2b; test_3b; test_4b; test_5b; test_6b |]

let () =
  test ta "Remove Dups From Sorted List (Func)";
  test tb "Remove Dups From Sorted Array (Imp)";