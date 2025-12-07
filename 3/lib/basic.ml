let maxChar = 95

let longestSubstring s =
  let l = String.length s in
  let h = Hashtbl.create (min maxChar l) in
  let best = ref 0 and start = ref 0 in
  for i = 0 to l - 1 do
    let c = String.get s i in
    match Hashtbl.find_opt h c with
    | None -> 
      Hashtbl.add h c i;
    | Some j -> 
      Hashtbl.replace h c i;
      if j >= !start then begin
        let size = i - !start in
        best := max !best size;
        start := j + 1
      end
  done;
  !best