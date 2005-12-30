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

(* The character set matches.  This is a hack to prevent us from printing
duplicate solutions.
*)
let matches = Hashtbl.create 1;;

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
let dictionary = Hashtbl.create 250000;;
let frequencies = Hashtbl.create 50;;

let want_view = ref false;;

(* Stuff we've seen *)
let is_a_word w =
	try
		ignore(Hashtbl.find dictionary w);
		true
	with Not_found -> false
;;


let load_words infile =
	let freqtmp = Hashtbl.create 50 in
	let record word =
		let freqs = (
			try
				Hashtbl.find freqtmp (String.length word)
			with Not_found ->
				let tmp = ref [] in
				Hashtbl.add freqtmp (String.length word) tmp;
				tmp
		) in
		try
			ignore(Hashtbl.find dictionary word)
		with Not_found ->
			Hashtbl.add dictionary word true;
			freqs := word :: !freqs;
	in
	let check_chars w =
		let rv = ref true in
		String.iter (fun c ->
			if(CharSet.mem c char_set) then () else (rv := false)) w;
		!rv
	in
	Fileutils.iter_file_lines (fun l ->
		if(check_chars l) then record l) infile;
	(* Reverse the word lists (we built them backwards) *)
	Hashtbl.iter (fun k v -> Hashtbl.add frequencies k (List.rev !v)) freqtmp;
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

(* Check if the mapping from s to d is available in the CharMap m.
   Either return a CharMap with this new mapping, or raise Not_found.

   A mapping is available if the source letter is not used as a key except
   to map to the destination letter, or the destination letter is not used as a
   destination unless mapped from the source letter.
 *)
let create_mapping s d m =
	CharMap.iter (fun k v ->
		if k = s && d <> v then raise Not_found;
		if d = v && k <> s then raise Not_found;
		) m;
	CharMap.add s d m
;;

(*
 This function takes two words:  a word from our cipher, and a possible word
 match.
 If the pattern can apply, a CharMap is returned mapping the input characters
 to the output characters that will convert strings of that input to the
 correct output.  If the pattern cannot apply, Not_found will be raised.

 Example:
 CharMap.empty iax cat
 will return a CharMap instance with i -> c, a -> a, x -> t

 CharMap.empty iix cat
 will raise Not_found because i can't map to both c and a
 *)
let make_map m input output =
	let rv = ref m in
	for i = 0 to (String.length input) - 1 do
		rv := create_mapping input.[i] output.[i] !rv
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
				rv := create_mapping orig.[i] output.[i] !rv;
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
	if not (Hashtbl.mem matches m) then (
		print_endline("Possible solution:");
		CharMap.iter (fun k v -> Printf.printf "%c=%c " k v) m;
		print_newline();
		List.iter (fun w -> print_endline("\t" ^ apply_map m w)) words;
		print_endline "-----------";
		Hashtbl.add matches m true;
	);
;;

(* Return true if these two strings match (with . as a wildcard in w) *)
let string_matches w fw =
	let rv = ref true in
	for i = 0 to (String.length w) - 1 do
		if fw.[i] <> w.[i] && w.[i] <> '.' then rv := false
	done;
	!rv
;;

let rec solve_rest m freqs words orig_words =
	if !want_view then (
		want_view := false;
		print_solution m orig_words;
	);
	match words with
	  [] -> if(FreqSet.cardinal freqs =
					(CharMap.fold (fun _ _ c -> c + 1) m 0)) then (
				if(all_are_words (List.map (apply_map m) orig_words)) then (
					print_solution m orig_words));
			m
	| h::t ->
		let w = apply_map m h in (
			if(try ignore(String.index w '.'); true with Not_found -> false)
			then (
				(* print_endline("\tChecking word " ^ w ^ " from " ^ h); *)
				let match_seen = ref false in
				let rec loop fwl =
					match fwl with
					[] -> m
					| fw::fwl2 ->
						if(string_matches w fw) then (
							match_seen := true;
							(* print_endline("\t\tmatch:  " ^ w ^ " ~ " ^ fw);
							*)
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
				solve_rest (loop (Hashtbl.find frequencies (String.length w)))
					freqs t orig_words
			) else (
				(* print_endline("\tWord is complete:  " ^ w); *)
				solve_rest m freqs t orig_words
			)
		);
;;

let solve freqs words orig_words =
	print_endline "Solving...";
	List.iter (fun w ->
			try
				let m = make_map CharMap.empty (List.hd words) w in
				(* print_endline("Starting word:  " ^ w); *)
				ignore(solve_rest m freqs (List.tl words) orig_words);
			with Not_found -> (); (* print_endline("NO MATCH"); *)
		) (Hashtbl.find frequencies (String.length (List.hd words)))
;;

let solve_hueristically words =
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

let solve_rotation words =
	let build_rotation_map n =
		let rec loop m c =
			let li = int_of_char c in
			if (li > int_of_char 'z') then
				m
			else (
				let ri = li + n in
				if (ri > (int_of_char 'z')) then (
					loop (CharMap.add c (char_of_int (ri - 26)) m)
						(char_of_int (succ li))
				) else (
					loop (CharMap.add c (char_of_int ri) m)
						(char_of_int (succ li))
				)
			)
		in loop CharMap.empty 'a'
	in
	for i = 0 to 25 do
		let m = build_rotation_map i in
		if(all_are_words (List.map (apply_map m) words)) then (
			print_endline("Solution at rot " ^ (string_of_int i));
			print_solution m words;
		)
	done;
;;

(* Initial thing:  hl fkzc vd lds *)
let main () =
	load_words "freqs.txt";
	let words = List.map String.lowercase (List.tl (Array.to_list Sys.argv)) in
	Sys.set_signal Sys.sigquit (
		Sys.Signal_handle(function s -> want_view := true));
	solve_rotation words;
	solve_hueristically words;
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;
