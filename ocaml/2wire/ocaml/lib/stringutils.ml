(*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: stringutils.ml,v 1.3 2002/12/11 09:49:58 dustin Exp $
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
let rec pvt_rec_split(buf, rv, str, c, i): '_a list =
	if i < String.length str then
	begin
		let ch = String.get str i in
		if ch = c then
			begin
				let s = (Buffer.contents buf) in
				Buffer.reset buf;
				pvt_rec_split(buf, (rv @ [ s ]), str, c,
					pvt_skip_char(str, c, i));
			end
		else
			begin
				Buffer.add_char buf ch;
				pvt_rec_split(buf, rv, str, c, (i + 1));
			end
	end
	else
		if String.length (Buffer.contents buf) > 0 then
			rv @ [ Buffer.contents buf ]
		else
			rv
;;

(* Split a string into a list of Strings *)
let split(s, c) =
	pvt_rec_split((Buffer.create 128), [], s, c, 0)
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
