(*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: stringutils.ml,v 1.4 2002/12/11 10:09:38 dustin Exp $
 *)

(* Private function to skip the next n occurrences of this character in
 * this stream.  Return the new offset.
 *)
let rec pvt_skip_char(s, c, i): int =
	if (i >= String.length s ) then
		String.length s
	else
		if ((String.get s i) = c) then
			pvt_skip_char(s, c, i+1)
		else
			i
;;

(* Private recursive function for splitting a stream in a buffer *)
let rec pvt_rec_split(rv, str, c, i): '_a list =
	if i < String.length str then
	begin
		if String.contains_from str i c then
		begin
			let o = String.index_from str i c in
			pvt_rec_split(
				(rv @ [ String.sub str i (o - i)]),
				str, c,
				pvt_skip_char(str, c, o))
		end
		else
			rv @ [ String.sub str i ((String.length str) - i) ]
	end
	else
		rv
;;

(* Split a string into a list of Strings *)
let split(s, c) =
	pvt_rec_split([], s, c, 0)
;;

(*
 * Test:
 * split("123 456   789", ' ');;
 * split("123 456   789  ", ' ');;
 *)

(* Locate a string in another string *)
let strstr(source, dest): int =
	5
;;
