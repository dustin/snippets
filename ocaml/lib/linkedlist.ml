(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: D078EA68-1252-11D8-9231-000393CFE6B8
 *)

(**
 A doubly linked list implementation.
 *)

(** Type for doubly linked lists. *)
type 'a node = {
	data: 'a;
	mutable prev: 'a node;
	mutable next: 'a node;
} and 'a t = { mutable l: 'a node option; mutable cnt: int }
;;

(** Exception thrown when certain operations are attempted on an empty list. *)
exception Empty_list;;

(** Create a node. *)
let create_node x =
	let tmp = { data=x; prev=Obj.magic 0; next=Obj.magic 0} in
	tmp.next <- tmp;
	tmp.prev <- tmp;
	tmp
;;

(** Get the head node of the given list. *)
let head_node l =
	match l.l with
	None -> raise Empty_list
	| Some el ->
		el
;;

(** Get the size of the given linked list. *)
let length l =
	l.cnt
;;

(** Create a linked list. *)
let create x =
	{ l = Some (create_node x); cnt=1 }
;;

(*
** Create an empty linked list. *
let create_empty =
	{ l = None; cnt=0; }
;;
*)

(** Append to this list.

@param x the value to append
@param l the list to which to append
*)
let append x l =
	match l.l with
	None ->
		let n = (create_node x) in
		l.l <- Some n;
		l.cnt <- 1;
		n;
	| Some el ->
		let tmp = create_node x in
		tmp.prev <- el.prev;
		tmp.next <- el;
		el.prev.next <- tmp;
		el.prev <- tmp;
		l.cnt <- l.cnt + 1;
		tmp;
;;

(** Iterate the given linked list. *)
let iter f l =
	match l.l with
	None -> ()
	| Some el ->
		let rec rec_iter ltmp =
			f ltmp.data;
			if ltmp.next != el then
				rec_iter ltmp.next;
			in
		rec_iter el
;;

(** Create a linked list from a list. *)
let create_from_list l =
	let rv = create (List.hd l) in
	List.iter (fun x -> ignore(append x rv)) (List.tl l);
	rv
;;

(** Convert this linked list into a regular list. *)
let to_list l =
	match l.l with
	None -> []
	| Some el ->
		let rec rec_to_list lcur rv =
			if lcur.next != el then
				lcur.data :: (rec_to_list lcur.next rv)
			else
				[lcur.data]
			in
		rec_to_list el []
;;

(** Delete this item from the list it contains. *)
let remove l it =
	if l.cnt = 0 then raise Empty_list;
	it.next.prev <- it.prev;
	it.prev.next <- it.next;
	match l.l with
	None -> ()
	| Some el ->
		if el = it then l.l <- Some el.next;
		l.cnt <- l.cnt - 1;
		()
;;

(** Pop the last item off the list. *)
let pop_last l =
	match l.l with
	None -> raise Empty_list
	| Some el ->
		let last = el.prev.data in
		remove l el.prev;
		last
;;

(** Pop the first item off the list. *)
let pop_first l =
	match l.l with
	None -> raise Empty_list
	| Some el ->
		let first = el.data in
		remove l el;
		first
;;
