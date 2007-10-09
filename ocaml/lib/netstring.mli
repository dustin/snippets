(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

(** Netstring implementation.

 {{:http://cr.yp.to/proto/netstrings.txt} http://cr.yp.to/proto/netstrings.txt}
 *)

val encode : string -> string

val decode : char Stream.t -> String.t
