(*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: stringutils.mli,v 1.2 2002/12/11 11:13:23 dustin Exp $
 *)

(* Split a string *)
val split : string char -> '_a array;;

(* Split a string on any character found in the provided list *)
val split_chars : string '_a array -> '_a array;;
