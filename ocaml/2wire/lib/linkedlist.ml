(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: D078EA68-1252-11D8-9231-000393CFE6B8
 *)

type 'a t = {
	data: 'a;
	mutable prev: 'a t option;
	mutable next: 'a t option;
}
;;

(** Create a linked list. *)
let create x = { data=x; next=None; prev=None };;

(** Prepend to this list.

@param x the value to prepend
@param l the list to which to prepend
@return the new list
*)
let prepend x l =
	let newnode = { data=x; next=None; prev=None } in
	l.prev <- Some newnode;
	newnode.next <- Some l;
	newnode
;;

(** Get the data from the head of this list. *)
let hd l = l.data ;;

let rec end_node l =
	match l.next with
		None -> l
		| Some(n) -> end_node n
;;

let append x l =
	(end_node l).next <- Some { data=x; next=None; prev=None };
	l
;;

let create_from_list l =
	let rec rec_cfl ll = function
		[] -> ll
		| h::t ->
			append h (rec_cfl ll t) in
	rec_cfl (create (List.hd l)) (List.tl l)
;;

(** Iterate the given linked list. *)
let rec iter f l =
	f l.data;
	match l.next with
		None -> ()
		| Some(n) -> iter f n;
;;

let debug_print = (fun x -> print_endline("Node: " ^ (string_of_int x)));;

let l = create 1;;

let l2 = prepend 2 l;;
