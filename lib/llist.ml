type 'a node = {
  content: 'a;
  mutable next: ('a node) option;
}

type 'a llist = {
  head: ('a node) option
}

let from_list l =
  let l' = List.rev l in
  let rec aux a = function
    | [] -> { head = a }
    | h::t -> aux (Some { content = h; next = a }) t
  in aux None l'

let to_list l =
  let rec aux a = function
    | None -> List.rev a
    | Some p -> aux (p.content :: a) p.next
  in aux [] l.head

let force_some n =
  match n with
  | None -> assert false
  | Some n' -> n'

let force_next n =
  let n' = force_some !n.next in
  n := n'

let safe_next n =
  match !n.next with
  | None -> ()
  | Some n' -> n := n'

let get_two_ahead n =
  match !n.next with
  | None -> assert false
  | Some n' -> n'.next