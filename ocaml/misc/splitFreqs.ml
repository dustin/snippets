(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 22E693A8-345E-11D8-B741-000393CB0F1E
 *)

(** Split a word frequency file. *)

module CharSet = Set.Make(Char);;

(* Hashing of the hash.  This gets us down to a maximum of 256 files. *)
let classification_hash word =
	(* This is the djb hashing algorithm.  Seems to do well *)
	let h = ref 5381 in
	String.iter (fun c -> h := ((!h lsl 5) + !h) + (int_of_char c)) word;
	Printf.sprintf "%02x" (!h land 0xff)
;;

let classify word =
	let rv = ref "" in
	let h = Hashtbl.create 1 in
	let maxl = ref (Char.code 'a') in
	String.iter (fun c ->
		rv := !rv ^ String.make 1 (
			try
				Hashtbl.find h c
			with Not_found ->
				Hashtbl.add h c (Char.chr !maxl);
				maxl := !maxl + 1;
				(Char.chr (!maxl - 1))
			)
		) word;
	!rv
;;

let hashed_classified word =
	classification_hash (classify word)
;;

let main () =
	let valid_chars = [
		'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
		'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z';
	] in
	let char_set = List.fold_left (fun c x -> CharSet.add x c)
		CharSet.empty valid_chars in
	let seen = Hashtbl.create 1 in
	let matches = Hashtbl.create 1 in
	let record word =
		Hashtbl.replace matches (hashed_classified word) (word ::
			try Hashtbl.find matches (hashed_classified word)
			with Not_found -> []
		)
	in
	let check_chars w =
		let rv = ref true in
		String.iter (fun c ->
			if(CharSet.mem c char_set) then () else (rv := false) ) w;
		!rv
	in
	Fileutils.conditional_iter_lines record check_chars stdin;
	Hashtbl.iter (fun k v ->
		let f = open_out (k ^ ".txt") in
		List.iter (fun w -> output_string f (w ^ "\n")) (List.rev v);
		close_out f
		) matches;
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;
