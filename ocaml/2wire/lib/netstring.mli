(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 2376C49D-2C20-11D8-B09D-000393CFE6B8
 *)

(** Netstring implementation.

 http://cr.yp.to/proto/netstrings.txt
 *)

val encode : string -> string

val decode : char Stream.t -> String.t
