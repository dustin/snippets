(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 97A1C814-2C7A-11D8-95E4-000393CB0F1E
 *)

(** Extra stream utilities. *)

val stream_convert : ('a -> 'b Stream.t) -> 'a Stream.t -> 'b Stream.t
