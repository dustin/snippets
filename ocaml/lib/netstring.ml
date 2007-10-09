(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

(** A netstring implementation. *)

(** Encode a string into a netstring. *)
let encode s =
	(string_of_int (String.length s)) ^ ":" ^ s ^ ","

(* The maximum length of a string we'll read
   (length of the max length of strings) *)
let max_string_length_length =
	String.length(string_of_int Sys.max_string_length)

(** Get the next netstring from the given stream. *)
let decode (in_stream: char Stream.t) =
	let sizebuf = (Buffer.create max_string_length_length) in
	let rec loop n =
		if (n > max_string_length_length) then
			failwith "Size too long";
		let c = Stream.next in_stream in
		if (c = ':') then (
			int_of_string (Buffer.contents sizebuf)
		) else (
			Buffer.add_char sizebuf c;
			loop (succ n)
		) in
	let size = loop 0 in
	if (size > Sys.max_string_length) then
		failwith ("Won't read a nestring of length " ^ (string_of_int size));
	let data = Stream.npeek size in_stream in
	List.iter (fun _ -> Stream.junk in_stream) data;
	let comma = Stream.next in_stream in
	if (comma != ',') then (
		failwith "Invalid netstring (didn't get a comma in the right place)"
	);
	Extstring.string_of_chars data
