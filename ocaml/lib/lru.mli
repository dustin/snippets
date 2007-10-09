(** LRU Caching. *)

type ('a, 'b) t = {
  keys : ('a, ('a, 'b) hval) Hashtbl.t;
  mutable seq : 'a Linkedlist.t option;
  func : 'a -> 'b;
  size : int;
}
and ('a, 'b) hval = { v : 'b; n : 'a Linkedlist.node; }
exception Illegal_state
val create : int -> ('a, 'b) t
val create_auto : int -> ('a -> 'b) -> ('a, 'b) t
val empty : ('a, 'b) t -> bool
val mem : ('a, 'b) t -> 'a -> bool
val remove : ('a, 'b) t -> 'a -> unit
val add : ('a, 'b) t -> 'a -> 'b -> unit
val find : ('a, 'b) t -> 'a -> 'b
val iter_keys : ('a, 'b) t -> ('a -> 'c) -> unit
