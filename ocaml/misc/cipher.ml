(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 66BD8522-343A-11D8-ABAC-000393CB0F1E
 *)

(* Simple cipher solver.  This should work for puzzles where there is a simple
letter replacement. *)

let single_letters = [
	'e'; 't'; 'o'; 'a'; 'n'; 'i'; 'r'; 's'; 'h'; 'd'; 'l'; 'c'; 'w';
	'u'; 'm'; 'f'; 'y'; 'g'; 'p'; 'b'; 'v'; 'k'; 'x'; 'q'; 'j'; 'z';
];;

module CharSet = Set.Make(Char);;

type char_freq = { letter: char; freq: int; };;

module Letter_freq = struct
	type t = char_freq
	let compare a b =
		if (a.freq = b.freq) then (
			compare a.letter b.letter
		) else (
			(* Highest frequency first *)
			compare b.freq a.freq
		)
end
;;

module FreqSet = Set.Make(Letter_freq);;

module CharMap = Map.Make(Char);;

(* The valid character set (for filtering word lists) *)
let char_set = List.fold_left (fun c x -> CharSet.add x c)
	CharSet.empty single_letters;;

let digraphs = [
	"th"; "er"; "on"; "an"; "re"; "he"; "in"; "ed"; "nd"; "ha"; "at"; "en";
	"es"; "of"; "or"; "nt"; "ea"; "ti"; "to"; "it"; "st"; "io"; "le"; "is";
	"ou"; "ar"; "as"; "de"; "rt"; "ve";
];;

let trigraphs = [
	"the"; "and"; "tha"; "ent"; "ion"; "tio"; "for"; "nde"; "has"; "nce";
	"edt"; "tis"; "oft"; "sth"; "men";
];;

let doubles = [
	"ss"; "ee"; "tt"; "ff"; "ll"; "mm"; "oo";
];;

let initial_letters = [
	't'; 'o'; 'a'; 'w'; 'b'; 'c'; 'd'; 's'; 'f'; 'm'; 'r'; 'h'; 'i'; 'y'; 'e';
	'g'; 'l'; 'n'; 'p'; 'u'; 'j'; 'k';
];;

let final_letters = [
	'e'; 's'; 't'; 'd'; 'n'; 'r'; 'y'; 'f'; 'l'; 'o'; 'g'; 'h'; 'a'; 'r'; 'm';
	'p'; 'u'; 'w';
];;

(* Dictionaries and stuff for checking the mappings *)
let dictionary = Hashtbl.create 1;;
let frequencies = Hashtbl.create 1;;

(* Stuff we've seen *)
let is_a_word w =
	try
		ignore(Hashtbl.find dictionary w);
		true
	with Not_found -> false
;;


let load_words infile =
	let record word =
		let freqs = (
			try
				Hashtbl.find frequencies (String.length word)
			with Not_found ->
				let tmp = ref [] in
				Hashtbl.add frequencies (String.length word) tmp;
				tmp
		) in
		try
			ignore(Hashtbl.find dictionary word)
		with Not_found ->
			Hashtbl.add dictionary word true;
			freqs := !freqs @ [word];
	in
	let check_chars w =
		let rv = ref true in
		String.iter (fun c ->
			if(CharSet.mem c char_set) then () else (rv := false)) w;
		!rv
	in
	Fileutils.iter_file_lines (fun l ->
		if(check_chars l) then record l) infile;
;;

let rec all_are_words l =
	match l with
	[] -> true
	| h::t ->
		if (is_a_word h) then
			all_are_words t
		else
			false
;;

let find_freq freqs c =
	FreqSet.fold (fun x rv -> if(x.letter = c) then x.freq else rv) freqs 0
;;

(* Get the significance of this word in our crypto gram *)
let word_weight freqs word =
	let rv = ref 0 in
	String.iter (fun c -> rv := !rv + ((find_freq freqs c) - 1)) word;
	!rv
;;

(* Count the frequencies of all of the letters in the provided words *)
let count_letter_freq words =
	let counts = Hashtbl.create 26 in
	List.iter (fun w ->
			String.iter (fun c ->
				try
					Hashtbl.replace counts c (1 + (Hashtbl.find counts c))
				with Not_found ->
					Hashtbl.add counts c 1
				)
			w
		) words;
	Hashtbl.fold (fun k v c ->
		FreqSet.add {letter=k; freq=v} c) counts FreqSet.empty;
;;

(* Word checker *)
let check_words words =
	print_endline "Are the words all words?";
	if (all_are_words words) then (
		print_endline "yes";
		true
	) else (
		print_endline "no";
		List.iter (fun w ->
			print_endline("\t" ^ w ^ ":\t"
				^ if(is_a_word w) then "word" else "not word")
		) words;
		false
	);
;;

let make_map m input output =
	let rv = ref m in
	for i = 0 to (String.length input) - 1 do
		rv := CharMap.add input.[i] output.[i] !rv
	done;
	!rv
;;

(* Like make_map, but will error on unavailable mappings *)
let make_map2 m orig input output =
	let rv = ref m in
	for i = 0 to (String.length input) - 1 do
		if (input.[i] = '.') then (
			if(CharMap.mem orig.[i] m) then (
				raise Not_found
			) else (
				rv := CharMap.add orig.[i] output.[i] !rv
			)
		)
	done;
	!rv
;;

let apply_map m word =
	let rv = String.copy word in
	for i = 0 to (String.length rv) - 1 do
		rv.[i] <- try
			CharMap.find rv.[i] m
			with Not_found -> '.'
	done;
	rv
;;

let print_solution m words =
	print_endline("Possible solution:");
	List.iter (fun w -> print_endline("\t" ^ apply_map m w)) words;
;;

let rec solve_rest m freqs words orig_words =
	match words with
	  [] -> if(FreqSet.cardinal freqs =
					(CharMap.fold (fun _ _ c -> c + 1) m 0)) then (
				print_solution m orig_words);
			m
	| h::t ->
		let w = apply_map m h in (
			if(try ignore(String.index w '.'); true with Not_found -> false)
			then (
				(* print_endline("\tChecking word " ^ w); *)
				let rx = Str.regexp w in
				let match_seen = ref false in
				let rec loop fwl =
					match fwl with
					[] -> m
					| fw::fwl2 ->
						if(Str.string_match rx fw 0) then (
							match_seen := true;
							(* print_endline("\t\tmatch:  " ^ fw); *)
							try
								solve_rest (make_map2 m h w fw)
									freqs t orig_words;
								loop fwl2;
							with Not_found ->
								(* print_endline("not found"); *)
								loop fwl2
						) else (
							loop fwl2
						);
						if(not !match_seen) then (raise Not_found) else m;
					in
				solve_rest (loop !(Hashtbl.find frequencies (String.length w)))
					freqs t orig_words
				(* solve_rest m t *)
			) else (
				(* print_endline("\tWord is complete:  " ^ w); *)
				solve_rest m freqs t orig_words
			)
		);
;;

let solve freqs words orig_words =
	print_endline "Solving...";
	List.iter (fun w -> let m = make_map CharMap.empty (List.hd words) w in
			(* print_endline("Starting word:  " ^ w); *)
			try
				ignore(solve_rest m freqs (List.tl words) orig_words);
				print_endline "-----------";
			with Not_found -> (); (* print_endline("NO MATCH"); *)
		) !(Hashtbl.find frequencies (String.length (List.hd words)))
;;

(* Initial thing:  hl fkzc vd lds *)
let main () =
	load_words "freqs.txt";
	let words = List.map String.lowercase (List.tl (Array.to_list Sys.argv)) in
	let freqs = count_letter_freq words in
	let weighed_words = List.sort (fun a b ->
		compare (word_weight freqs b) (word_weight freqs a)) words in
	print_endline("Letter frequencies:");
	FreqSet.iter (fun i -> Printf.printf "\t%c -> %d\n" i.letter i.freq) freqs;
	print_endline("Sorted by weight:");
	List.iter (fun w -> Printf.printf "\t%s weighs %d\n"
		w (word_weight freqs w)) weighed_words;
	solve freqs weighed_words words;
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;
