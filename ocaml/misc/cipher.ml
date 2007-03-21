(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 66BD8522-343A-11D8-ABAC-000393CB0F1E
 *)

(* Simple cipher solver.  This should work for puzzles where there is a simple
letter replacement. *)

open Unix

let datadir =
	try getenv "CIPHER_DATADIR"
	with Not_found -> ",tmp"

let single_letters = [
	'e'; 't'; 'o'; 'a'; 'n'; 'i'; 'r'; 's'; 'h'; 'd'; 'l'; 'c'; 'w';
	'u'; 'm'; 'f'; 'y'; 'g'; 'p'; 'b'; 'v'; 'k'; 'x'; 'q'; 'j'; 'z'; 
]

module CharSet = Set.Make(Char)

type char_freq = { letter: char; freq: int; }

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

module FreqSet = Set.Make(Letter_freq)

module CharMap = Map.Make(Char)

(* The character set matches.  This is a hack to prevent us from printing
duplicate solutions.
*)
let matches = Hashtbl.create 1

(* The valid character set (for filtering word lists) *)
let char_set = List.fold_left (fun c x -> CharSet.add x c)
	CharSet.empty single_letters

let digraphs = [
	"th"; "er"; "on"; "an"; "re"; "he"; "in"; "ed"; "nd"; "ha"; "at"; "en";
	"es"; "of"; "or"; "nt"; "ea"; "ti"; "to"; "it"; "st"; "io"; "le"; "is";
	"ou"; "ar"; "as"; "de"; "rt"; "ve";
]

let trigraphs = [
	"the"; "and"; "tha"; "ent"; "ion"; "tio"; "for"; "nde"; "has"; "nce";
	"edt"; "tis"; "oft"; "sth"; "men";
]

let doubles = [
	"ss"; "ee"; "tt"; "ff"; "ll"; "mm"; "oo";
]

let initial_letters = [
	't'; 'o'; 'a'; 'w'; 'b'; 'c'; 'd'; 's'; 'f'; 'm'; 'r'; 'h'; 'i'; 'y'; 'e';
	'g'; 'l'; 'n'; 'p'; 'u'; 'j'; 'k';
]

let final_letters = [
	'e'; 's'; 't'; 'd'; 'n'; 'r'; 'y'; 'f'; 'l'; 'o'; 'g'; 'h'; 'a'; 'r'; 'm';
	'p'; 'u'; 'w';
]

(* Dictionaries and stuff for checking the mappings *)
let dictionary = Hashtbl.create 250000
let frequencies = Hashtbl.create 50

let want_view = ref false

(* Stuff we've seen *)
let is_a_word w =
	try
		ignore(Hashtbl.find dictionary w);
		true
	with Not_found -> false

(* This is used to locate word files *)
let classification_hash word =
	(* This is the djb hashing algorithm.  Seems to do well *)
	let h = ref 5381 in
	String.iter (fun c -> h := ((!h lsl 5) + !h) lxor (int_of_char c)) word;
	Printf.sprintf "%02x" (!h land 0xff)

(* Word classification for match lookups (not very functional) *)
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

(* Memoization over the classification *)
let classify_memo = Hashtbl.create 1;;
let classifym word =
	try Hashtbl.find classify_memo word
	with Not_found ->
		let c = classify word in
		Hashtbl.add classify_memo word c;
		c

let load_words infile =
	Printf.printf "Loading words from %s\n" infile;
	let freqtmp = Hashtbl.create 50 in
	let record word =
		let freqs = (
			try
				Hashtbl.find freqtmp (classify word)
			with Not_found ->
				let tmp = ref [] in
				Hashtbl.add freqtmp (classify word) tmp;
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
	Hashtbl.iter (fun k v -> Hashtbl.add frequencies k (List.rev !v)) freqtmp

let rec all_are_words l =
	match l with
	[] -> true
	| h::t ->
		if (is_a_word h) then
			all_are_words t
		else
			false

let find_freq freqs c =
	FreqSet.fold (fun x rv -> if(x.letter = c) then x.freq else rv) freqs 0

(* Get the significance of this word in our crypto gram *)
let word_weight freqs word =
	let rv = ref 0 in
	String.iter (fun c -> rv := !rv + ((find_freq freqs c) - 1)) word;
	!rv

(* Get the weight by classification *)
let classification_weight word =
	try
		List.length (Hashtbl.find frequencies (classifym word))
	with Not_found ->
		Printf.eprintf "%s's classification (%s) matches no known word\n"
			word (classifym word);
		raise Not_found

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
		FreqSet.add {letter=k; freq=v} c) counts FreqSet.empty

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
	)

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

let apply_map m word =
	let rv = String.copy word in
	for i = 0 to (String.length rv) - 1 do
		rv.[i] <- try
			CharMap.find rv.[i] m
			with Not_found -> '.'
	done;
	rv

let print_solution m words =
	let key = CharMap.fold (fun k v i -> i ^ (Printf.sprintf "%c=%c " k v)) m ""
		in
	if not (Hashtbl.mem matches key) then (
		let pt = times() in
		Printf.printf "Possible solution (u=%.2f s=%.2f):\n%s\n"
			pt.tms_utime pt.tms_stime key;
		List.iter (fun w -> print_endline("\t" ^ apply_map m w)) words;
		Printf.printf "-----------\n%!";
		Hashtbl.add matches key true;
	)

(* Return true if these two strings match (with . as a wildcard in w) *)
let string_matches w fw =
	let rv = ref true in
	for i = 0 to (String.length w) - 1 do
		if fw.[i] <> w.[i] && w.[i] <> '.' then rv := false
	done;
	!rv

let rec solve_rest m freqs words orig_words =
	if !want_view then (
		print_solution m orig_words;
		want_view := false;
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
				solve_rest (loop (Hashtbl.find frequencies (classifym h)))
					freqs t orig_words
			) else (
				(* print_endline("\tWord is complete:  " ^ w); *)
				solve_rest m freqs t orig_words
			)
		)

let solve freqs words orig_words =
	let t = times() in
	Printf.printf "Solving as of u=%.2f s=%.2f...\n%!" t.tms_utime t.tms_stime;
	List.iter (fun w ->
			try
				let m = make_map CharMap.empty (List.hd words) w in
				(* print_endline("Starting word:  " ^ w); *)
				ignore(solve_rest m freqs (List.tl words) orig_words);
			with Not_found -> (); (* print_endline("NO MATCH"); *)
		) (Hashtbl.find frequencies (classifym (List.hd words)))

(* This is a fairly complicated weighted sort that tries to search words with
 the least search space first (by order of magnitude only) followed by best
 weighted match, and then exact search space *)
let complicated_weight_sorting freqs a b =
	let ca = (classification_weight a) in
	let cb = (classification_weight b) in
	let cw = compare (int_of_float (log (float_of_int ca)))
		(int_of_float (log (float_of_int cb))) in
	if cw <> 0 then (
		cw
	) else (
		let ww = compare (word_weight freqs b) (word_weight freqs a) in
		if ww <> 0 then ww
		else compare ca cb
	)

let solve_heuristically words =
	let freqs = count_letter_freq words in
	(* Remove all of the duplicates before getting the weighted list *)
	let remh = Hashtbl.create 1 in
	List.iter (fun w -> Hashtbl.replace remh w true) words;
	let uniq_words = Hashtbl.fold (fun a b i -> a :: i) remh [] in
	(* Get the words weighted on whatever weight sorting algorithm we use *)
	let weighed_words1 =
		List.sort (complicated_weight_sorting freqs) uniq_words in
	(* Remove any words that don't contribute to solving the problem *)
	let seen_letters = Hashtbl.create 1 in
	let worth_map = Hashtbl.create 1 in
	List.iter (fun w ->
		let worth = ref 0 in
		String.iter (fun c ->
			if not (Hashtbl.mem seen_letters c) then (
				worth := !worth + 1;
				Hashtbl.add seen_letters c true
			)
		) w;
		Hashtbl.add worth_map w !worth;
		) weighed_words1;
	(* We keep anything with a worth >= 1 *)
	let weighed_words = List.filter (fun w -> (Hashtbl.find worth_map w) > 0)
		weighed_words1 in
	(* Display the stuff *)
	print_endline("Letter frequencies:");
	FreqSet.iter (fun i -> Printf.printf "\t%c -> %d\n" i.letter i.freq) freqs;
	print_endline("Contributing words sorted by weight:");
	List.iter (fun w -> Printf.printf
		"\t%s weighs %d, %d unique letters, %d class matches (%s)\n"
		w (word_weight freqs w) (Hashtbl.find worth_map w)
		(classification_weight w) (classifym w))
		weighed_words;
	Printf.printf "%!";
	solve freqs weighed_words words

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
	done

(* Initial thing:  hl fkzc vd lds *)
let main () =
	let words = List.map String.lowercase (List.tl (Array.to_list Sys.argv)) in
	(* Calculate the dictionary subsets that need to be loaded *)
	let files = Hashtbl.create 1 in
		List.iter (fun w -> Hashtbl.replace files
			(Printf.sprintf "%s/%s.txt" datadir
				(classification_hash (classify w))) true) words;
	Hashtbl.iter (fun k v -> load_words k) files;

	Sys.set_signal Sys.sigquit (
		Sys.Signal_handle(function s -> want_view := true));
	solve_rotation words;
	solve_heuristically words
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
