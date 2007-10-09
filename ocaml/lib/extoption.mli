(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

(** Extra option utilities. *)

exception Empty_option
val is_none : 'a option -> bool
val is_some : 'a option -> bool
val get_option : 'a option -> 'a
