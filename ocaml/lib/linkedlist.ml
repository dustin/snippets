(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: D078EA68-1252-11D8-9231-000393CFE6B8
 *)

type 'a t = {
	data: 'a;
	mutable prev: 'a t;
	mutable next: 'a t;
}
;;

(** Exception thrown when a single-item list is attempted to be emptied. *)
exception Empty_list;;

(** Create a linked list. *)
let create x =
	let tmp = { data=x; prev=Obj.magic 0; next=Obj.magic 0} in
	tmp.next <- tmp;
	tmp.prev <- tmp;
	tmp
;;

(** Append to this list.

@param x the value to append
@param l the list to which to append
*)
let append x l =
	let tmp = create x in
	tmp.prev <- l.prev;
	tmp.next <- l;
	l.prev.next <- tmp;
	l.prev <- tmp;
;;

(** Iterate the given linked list. *)
let iter f l =
	let rec rec_iter ltmp start =
		f ltmp.data;
		if ltmp.next != start then
			rec_iter ltmp.next start;
		in
	rec_iter l l
;;

(** Create a linked list from a list. *)
let create_from_list l =
	let rv = create (List.hd l) in
	List.iter (fun x -> append x rv) (List.tl l);
	rv
;;

(** Delete this item from the list it contains. *)
let delete_item l =
	if (l.prev = l) then raise Empty_list;
	l.next.prev <- l.prev;
	l.prev.next <- l.next;
	l.next;
;;

let debug_print = (fun x -> print_endline("Node: " ^ (string_of_int x)));;
