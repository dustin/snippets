(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 021D46D0-2C77-11D8-BB07-000393CB0F1E
 *)

(** Extra option utilities. *)

exception Empty_option
val is_none : 'a option -> bool
val is_some : 'a option -> bool
val get_option : 'a -> 'b option -> 'b
