(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: C9A92082-10BD-11D8-81F6-000393CFE6B8
 *)

(**
 LRU caching.
 *)

(**
 The lru cache type.
 *)
type ('a, 'b) t = {
	(* The keys -> value mappings *)
	keys: ('a, ('a, 'b) hval) Hashtbl.t;
	(* The sequences (for aging) *)
	mutable seq: 'a Linkedlist.t option;
	(* autofetch function *)
	func: ('a -> 'b);
	(* The maximum size of this thing *)
	size: int;
} and ('a, 'b) hval = {
	v: 'b;
	n: 'a Linkedlist.node;
}

(** Illegal state exceptions. *)
exception Illegal_state

(**
 Create an LRU cache with the given maximum size.

 @param max_size the maximum size of this LRU cache
 *)
let create max_size = {
	keys = Hashtbl.create 1;
	seq = None;
	func = (fun x -> raise Not_found);
	size = max_size;
}

(**
 Create an LRU cache with an auto creation function.

 @param max_size the maximum size of the function
 @param f the auto-populate function
 *)
let create_auto max_size f = {
	keys = Hashtbl.create 1;
	seq = None;
	func = f;
	size = max_size;
}

(**
 Clear out this LRU cache.

 @param lru the LRU cache to clear
 *)
let empty lru =
	Hashtbl.clear lru.keys;
	lru.seq = None

(**
 Is this object a member of this LRU cache?

 @param lru the LRU cache to check membership of
 @param k the object to look for
 *)
let mem lru k =
	Hashtbl.mem lru.keys k

(**
 Remove an object from the LRU cache.

 @param lru the LRU cache that wants something removed.
 @param k the key of the item to remove
 *)
let remove lru k =
	try
		match lru.seq with
			None -> ()
			| Some(ll) ->
				let it = Hashtbl.find lru.keys k in
				Hashtbl.remove lru.keys k;
				Linkedlist.remove ll it.n;
				()
	with Not_found -> ()

let remove_if_full lru =
	match lru.seq with
	None -> ()
	| Some(ll) ->
		if ((Linkedlist.length ll) >= lru.size) then
			let f = Linkedlist.pop_first ll in
			Hashtbl.remove lru.keys f;
		()

(**
 Add this item to the cache.

 @param lru the LRU cache to receive the item
 @param k the key for this item
 @param v the value of the item
 *)
let add lru k v =
	match lru.seq with
		None ->
			let tmpseq = Linkedlist.create k in
			let head = Linkedlist.head_node tmpseq in
				lru.seq <- Some tmpseq;
				Hashtbl.add lru.keys k {n=head; v=v}
		| Some(ll) ->
			remove lru k;
			remove_if_full lru;
			let node = Linkedlist.append k ll in
			Hashtbl.add lru.keys k {n=node; v=v}

(**
 Get an object from the LRU cache.  This also updates the usage sequence thing.

 @param lru the LRU cache from which to extract the object
 @param k the key of the object to get
 *)
let find lru k =
	try
		let rv = Hashtbl.find lru.keys k in
		(* Move it to the front *)
		match lru.seq with
			None -> raise Illegal_state
			| Some(ll) ->
				add lru k rv.v;
				rv.v
	with Not_found ->
		let rv = lru.func k in
		add lru k rv;
		rv

(**
 Apply the given function all keys in this cache.
 *)
let iter_keys lru f =
	match lru.seq with
	None -> ()
	| Some(ll) -> Linkedlist.iter f ll
