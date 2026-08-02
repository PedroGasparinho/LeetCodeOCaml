let rec count s i b a l o n =
  if i < 0 || i >= String.length s then begin
    min b a
    |> min (l/2)
    |> min (o/2)
    |> min n
  end else begin
    let aux = count s (i+1) in
    match s.[i] with
      | 'b' -> aux (b+1) a l o n
      | 'a' -> aux b (a+1) l o n
      | 'l' -> aux b a (l+1) o n
      | 'o' -> aux b a l (o+1) n
      | 'n' -> aux b a l o (n+1)
      | _ -> aux b a l o n
  end

let main s =
  count s 0 0 0 0 0 0