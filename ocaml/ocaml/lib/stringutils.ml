(*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: stringutils.ml,v 1.2 2002/12/11 09:18:01 dustin Exp $
 *)

(* Private function to skip the next n occurrences of this character in
 * this stream
 *)
let pvt_skip_char(t, c) =
	while (Stream.npeek 1 t) = [ c ] do
		Stream.next t
	done
;;

(* Private recursive function for splitting a stream in a buffer *)
let rec pvt_rec_split_stream(str, rv, t, c): '_a list =
	if Stream.peek t != None then
	begin
		let ch = Stream.next t in
		if ch = c then
			begin
				pvt_skip_char(t, c);
				pvt_rec_split_stream("", (rv @ [ str ]), t, c);
			end
		else
			begin
				pvt_rec_split_stream(str ^ (String.make 1 ch), rv, t, c);
			end
	end
	else
		rv @ [ str ]
;;

(* Split a stream on a character. *)
let split_stream(t, c): '_a list =
	pvt_rec_split_stream("", [], t, c)
;;

(* Split a string into a list of Strings *)
let split(s, c) =
	split_stream(Stream.of_string(s), c)
;;

(* Locate a string in another string *)
let strstr(source, dest): int =
	5
;;
