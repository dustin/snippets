(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 1A5344F4-2C77-11D8-B2DA-000393CB0F1E
 *)

exception Empty_option;;

(** Is this option none? *)
let is_none = function
	  None -> true
	| _ -> false

(** Is this option some? *)
let is_some = function
	  Some(x) -> true
	| _ -> false

(** Get an option value. *)
let get_option = function
	  Some(x) -> x
	| None -> raise Empty_option
