(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
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
