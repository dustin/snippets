(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: D078EA68-1252-11D8-9231-000393CFE6B8
 *)

type 'a node = {
	data: 'a;
	mutable prev: 'a node;
	mutable next: 'a node;
} and 'a t = { mutable l: 'a node; mutable cnt: int }
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
	if (l.cnt = 0) then raise Empty_list
	else l.l
;;

(** Get the size of the given linked list. *)
let size l =
	l.cnt
;;

(** Create a linked list. *)
let create x =
	{ l = create_node x; cnt=1 }
;;

(** Append to this list.

@param x the value to append
@param l the list to which to append
*)
let append x l =
	if l.cnt = 0 then
	begin
		l.l <- create_node x;
		l.cnt <- 1;
		l.l;
	end
	else
	begin
		let tmp = create_node x in
		tmp.prev <- l.l.prev;
		tmp.next <- l.l;
		l.l.prev.next <- tmp;
		l.l.prev <- tmp;
		l.cnt <- l.cnt + 1;
		tmp;
	end
;;

(** Iterate the given linked list. *)
let iter f l =
	if l.cnt = 0 then
		()
	else
	begin
		let rec rec_iter ltmp =
			f ltmp.data;
			if ltmp.next != l.l then
				rec_iter ltmp.next;
			in
		rec_iter l.l
	end
;;

(** Create a linked list from a list. *)
let create_from_list l =
	let rv = create (List.hd l) in
	List.iter (fun x -> ignore(append x rv)) (List.tl l);
	rv
;;

(** Delete this item from the list it contains. *)
let remove l it =
	if l.cnt = 0 then raise Empty_list;
	it.next.prev <- it.prev;
	it.prev.next <- it.next;
	if l.l = it then l.l <- l.l.next;
	l.cnt <- l.cnt - 1;
	()
;;

(** Pop the last item off the list. *)
let pop_last l =
	let last = l.l.prev.data in
	remove l l.l.prev;
	last
;;

(** Pop the first item off the list. *)
let pop_first l =
	let first = l.l.data in
	remove l l.l;
	first
;;

let debug_print = (fun x -> print_endline("Node: " ^ (string_of_int x)));;
