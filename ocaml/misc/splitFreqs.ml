(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 22E693A8-345E-11D8-B741-000393CB0F1E
 *)

(** Split a word frequency file. *)

(*
6280 807 symmetry n
268723 been vbn 3975
*)

module CharSet = Set.Make(Char);;

let main() =
	let valid_chars = [
		'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
		'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z';
	] in
	let char_set = List.fold_left (fun c x -> CharSet.add x c)
		CharSet.empty valid_chars in
	let seen = Hashtbl.create 1 in
	let files = Hashtbl.create 1 in
	let record word =
		let f = (
			try
				Hashtbl.find files (String.length word)
			with Not_found ->
				let tmp = open_out ((string_of_int (String.length word))
					^ ".txt") in
				Hashtbl.add files (String.length word) tmp;
				tmp
		) in
		try
			ignore(Hashtbl.find seen word)
		with Not_found ->
			Hashtbl.add seen word true;
			output_string f (word ^ "\n")
	in
	let check_chars w =
		let rv = ref true in
		String.iter (fun c ->
			if(CharSet.mem c char_set) then () else (rv := false) ) w;
		!rv
	in
	Fileutils.conditional_iter_lines record check_chars stdin;
	Hashtbl.iter (fun k v -> close_out v) files;
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;
