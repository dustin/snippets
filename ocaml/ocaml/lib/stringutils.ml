(*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: stringutils.ml,v 1.6 2002/12/11 23:02:45 dustin Exp $
 *)

(* Private function to skip the next n occurrences of any character in this
 * list in this string.  Return the new offset.
 *)
let rec pvt_skip_chars(s, l, i): int =
	if (i >= String.length s ) then
		String.length s
	else
		if (List.mem (String.get s i) l) then
			pvt_skip_chars(s, l, i+1)
		else
			i
;;

(* Get the position of the given character in the given string, or -1 *)
let str_index_of_char(str, c, i): int =
	if String.contains_from str i c then
		String.index_from str i c
	else
		-1
;;

(* Get the first non-negative number from this list *)
(* test:  pvt_min([-1; 8; 3; 2; -1], -1);; *)
let rec pvt_min(l, minsaw): int =
	if l = [] then
		minsaw
	else
		if (List.hd l = -1) then
			pvt_min(List.tl l, minsaw)
		else
			if minsaw < 0 then
				pvt_min(List.tl l, List.hd l)
			else
				if (List.hd l >= 0) && (List.hd l < minsaw) then
					pvt_min(List.tl l, List.hd l)
				else
					pvt_min(List.tl l, minsaw)
;;

(* Find the index of one of these characters, or -1 if it doesn't exist *)
let str_index_of_one(str, l, i): int =
	pvt_min((List.map (function x -> str_index_of_char(str, x, i)) l), -1)
;;

(* Private recursive function for splitting a stream in a buffer *)
let rec pvt_rec_split_chars(rv, str, l, i, limit): '_a list =
	if (List.length rv < (limit-1)) && (i < String.length str) then
	begin
		let pos = str_index_of_one(str, l, i) in
		if pos != -1 then
			pvt_rec_split_chars(
				(rv @ [ String.sub str i (pos - i)]),
				str, l,
				pvt_skip_chars(str, l, pos), limit)
		else
			rv @ [ String.sub str i ((String.length str) - i) ]
	end
	else
		if i < String.length str then
			rv @ [ String.sub str i ((String.length str) - i) ]
		else
			rv
;;

(* Private function to skip the next n occurrences of this character in
 * this string.  Return the new offset.
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
let rec pvt_rec_split(rv, str, c, i, limit): '_a list =
	if (List.length rv < (limit - 1)) && (i < String.length str) then
	begin
		if String.contains_from str i c then
		begin
			let o = String.index_from str i c in
			pvt_rec_split(
				(rv @ [ String.sub str i (o - i)]),
				str, c,
				pvt_skip_char(str, c, o), limit)
		end
		else
			rv @ [ String.sub str i ((String.length str) - i) ]
	end
	else
		if i < String.length str then
			rv @ [ String.sub str i ((String.length str) - i) ]
		else
			rv
;;

(* Split a string into a list of Strings *)
let split(s, c, limit) =
	pvt_rec_split([], s, c, 0, limit)
;;

(* Split a string into a list of Strings *)
let split_chars(s, l, limit) =
	pvt_rec_split_chars([], s, l, 0, limit)
;;

(*
 * Test:
 * split("123 456   789", ' ', 99);;
 * split("123 456   789  ", ' ', 99);;
 * split("123 456   789  ", ' ', 2);;
 * split_chars("123:456- -789", [':'; ' '; '-'], 99);;
 *)

(* Locate a string in another string *)
let strstr(source, dest): int =
	5
;;
