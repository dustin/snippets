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
	keys: ('a, 'b) Hashtbl.t;
	(* The sequences (for aging) *)
	mutable seq: 'a list;
	(* The maximum size of this thing *)
	size: int;
};;

(**
 Create an LRU cache with the given maximum size.

 @param max_size the maximum size of this LRU cache
 *)
let create max_size = {
	keys = Hashtbl.create 1;
	seq = [];
	size = max_size;
};;

(**
 Clear out this LRU cache.

 @param lru the LRU cache to clear
 *)
let empty lru =
	Hashtbl.clear lru.keys;
	lru.seq = [];
;;

(**
 Is this object a member of this LRU cache?

 @param lru the LRU cache to check membership of
 @param k the object to look for
 *)
let mem lru k =
	Hashtbl.mem lru.keys k
;;

(**
 Get an object from the LRU cache.

 @param lru the LRU cache from which to extract the object
 @param k the key of the object to get
 *)
let find lru k =
	Hashtbl.find lru.keys k
;;

(**
 Remove an object from the LRU cache.

 @param lru the LRU cache that wants something removed.
 @param k the key of the item to remove
 *)
let remove lru k =
	Hashtbl.remove lru.keys k;
	lru.seq <- List.filter (fun x -> x != k) lru.seq;
	()
;;

(**
 Add this item to the cache.

 @param lru the LRU cache to receive the item
 @param k the key for this item
 @param v the value of the item
 *)
let add lru k v =
	(* If the cache is full, remove an item *)
	if ( (List.length lru.seq) > lru.size) then
		remove lru (List.hd lru.seq);
	(* If this key already exists, remove it *)
	if (mem lru k) then
		(remove lru k);
	(* Add it *)
	Hashtbl.add lru.keys k v;
	lru.seq <- List.append lru.seq [k];
	()
;;
