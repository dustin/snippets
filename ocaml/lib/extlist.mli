(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

(** Extra list utilities. *)

val iteri : (int -> 'a -> unit) -> 'a list -> unit
val shuffle : 'a list -> 'a list
val zip : 'a list list -> 'a list list
val nthtail : 'a list -> int -> 'a list
