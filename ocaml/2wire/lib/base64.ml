(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 2018780E-294F-11D8-9B49-000393CB0F1E
 *)

(**
 Base64 block encoder/decoder.
 *)

(** Exception raised when there's an attempt to encode a chunk incorrectly *)
exception Invalid_chunk;;

(** The character map of all base64 characters *)
let char_map = [|
	'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M';
	'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z';
	'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
	'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z';
	'0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '+'; '/'|]
;;

(** Reverse mapping of character to its index in the char_map *)
let char_index =
	let rv = Array.make 256 (-1) in
	for i = 0 to (Array.length char_map - 1) do
		let c = char_map.(i) in
		Array.set rv (Char.code c) i
	done;
	rv
;;

(** Is the given character a valid base64 character? *)
let is_base64_char c =
	char_index.(Char.code c) != -1
;;

(** Encode a chunk.  The chunk is either a 1, 2, or 3 element list. *)
let encode_chunk chars =
	if(List.length chars = 0 || List.length chars > 3) then
		raise Invalid_chunk;
	let chunk = String.create 4 in
	let a = List.nth chars 0 in
	let tmpa = (((Char.code a) land 3) lsl 4) in
	chunk.[0] <- char_map.( (Char.code a) lsr 2);
	(* Check for another character *)
	if (List.length chars < 2) then (
		chunk.[1] <- char_map.(tmpa);
		chunk.[2] <- '=';
		chunk.[3] <- '=';
		chunk;
	) else (
		let b = List.nth chars 1 in
		let tmpb = ((Char.code b) lsr 4) in
		let tmpa2 = ((Char.code b) land 0x0f) lsl 2 in
		chunk.[1] <- char_map.(tmpa lor tmpb);
		if (List.length chars < 3) then (
			chunk.[2] <- char_map.(tmpa2);
			chunk.[3] <- '=';
			chunk
		) else (
			let c = List.nth chars 2 in
			let tmpb2 = ((Char.code c) land 0xc0) lsr 6 in
			chunk.[2] <- char_map.(tmpa2 lor tmpb2);
			chunk.[3] <- char_map.((Char.code c) land 0x3f);
			chunk
		)
	);
;;

(** Stream chunk encoder. *)
let stream_encode data_stream cnt =
	let stream_empty s =
		try
			Stream.empty s;
			true
		with Stream.Failure -> false in
	if (stream_empty data_stream) then (
		None
	) else (
		let next = Stream.npeek 3 data_stream in
		List.iter (fun x -> Stream.junk data_stream) next;
		(* We don't do 76 here as they're in blocks of 4. *)
		Some (encode_chunk next ^
			(if (((cnt + 1) mod 19) = 0) then "\r\n" else ""))
	)
;;

(**
 Base64 encode the string data into a base64 encoded string.
 *)
let encode data_stream =
	let stream_empty s =
		try
			Stream.empty s;
			true
		with Stream.Failure -> false in
	let rec coder cnt rv =
		if (stream_empty data_stream) then (
			rv
		) else (
			let next = Stream.npeek 3 data_stream in
			List.iter (fun x -> Stream.junk data_stream) next;
			coder (cnt + 4) (rv ^ encode_chunk next ^
				(if (((cnt + 4) mod 76) = 0) then "\r\n" else ""))
		) in
	coder 0 ""
;;

(** Base64 encode a string *)
let encode_string s = encode (Stream.of_string s);;

(** Simple test function. *)
let test() =
	let wordlist = ["A";"AB";"ABC";"Dustin";String.create 128] in
	print_endline("String:");
	List.iter (fun x -> print_endline(encode_string x))
		wordlist;
	print_endline("Stream:");
	List.iter (fun x ->
			Stream.iter print_string
				(Stream.from (stream_encode (Stream.of_string x)));
			print_newline()
		) wordlist;
;;
