(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

(** Extra stream utilities. *)

val stream_convert : ('a -> 'b Stream.t) -> 'a Stream.t -> 'b Stream.t
