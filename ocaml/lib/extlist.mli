(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 05F6932A-2C76-11D8-AF98-000393CB0F1E
 *)

(** Extra list utilities. *)

val iteri : (int -> 'a -> unit) -> 'a list -> unit
val shuffle : 'a list -> 'a list
