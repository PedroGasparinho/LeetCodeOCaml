type 'a node = {
  content: 'a;
  mutable next: ('a node) option;
}

type 'a llist = {
  mutable head: ('a node) option
}

val from_list : 'a list -> 'a llist
val to_list : 'a llist -> 'a list
val force_some : 'a node option -> 'a node
val force_next: 'a node ref -> unit
val safe_next: 'a node ref -> unit
val get_two_ahead: 'a node ref -> 'a node option