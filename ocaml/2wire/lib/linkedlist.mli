(** A doubly linked list implementation.  *)

type 'a node = { data : 'a; mutable prev : 'a node; mutable next : 'a node; }
and 'a t = { mutable l : 'a node option; mutable cnt : int; }
exception Empty_list
val create_node : 'a -> 'a node
val head_node : 'a t -> 'a node
val length : 'a t -> int
val create : 'a -> 'a t
val append : 'a -> 'a t -> 'a node
val iter : ('a -> 'b) -> 'a t -> unit
val create_from_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val remove : 'a t -> 'a node -> unit
val pop_last : 'a t -> 'a
val pop_first : 'a t -> 'a

(*
 * arch-tag: 55F86976-2B50-11D8-AFDE-000393CFE6B8
 *)
