(*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: stringutils.ml,v 1.1 2002/12/11 07:46:36 dustin Exp $
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
let rec pvt_rec_split_stream(buf, rv, t, c): '_a list =
	try
		let ch = Stream.next t in
		if ch = c then
			begin
				pvt_skip_char(t, c);
				let s = (Buffer.contents buf) in
				Buffer.reset buf;
				pvt_rec_split_stream(buf, (rv @ [ s ]), t, c);
			end
		else
			begin
				Buffer.add_char buf ch;
				pvt_rec_split_stream(buf, rv, t, c);
			end
	with Stream.Failure -> ignore();
	rv @ [ Buffer.contents buf ]
;;

(* Split a stream on a character. *)
let split_stream(t, c): '_a list =
	pvt_rec_split_stream((Buffer.create 16), [], t, c)
;;

(* Split a string into a list of Strings *)
let split(s, c) =
	split_stream(Stream.of_string(s), c)
;;

(* Locate a string in another string *)
let strstr(source, dest): int =
	5
;;
