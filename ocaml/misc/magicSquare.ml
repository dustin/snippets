(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 01D5001A-2173-11D8-907F-00039359E8C6
 *)

exception Validation_failure of string;;

let size = ref 3;;

(* Translate the flat array to rows and columns *)
let get_row m x =
	let base = x * (!size) in
	let rv = ref [] in
	for i = 0 to (!size - 1) do
		rv := !rv @ [List.nth m (base + i)]
	done;
	!rv
;;
let get_col m x =
	let rv = ref [] in
	for i = 0 to (!size - 1) do
		rv := !rv @ [List.nth m (x + (i * !size))]
	done;
	!rv
;;

(** Skip part of a list *)
let rec list_skip m n =
	if (n > 0) then
		list_skip (List.tl m) (n - 1)
	else
		m
;;

(* Get the top left to bottom right diagonal *)
let get_diag_down m =
	let skip_size = (!size + 1) in
	let rec fetch_down rv m =
	if (List.length rv = !size) then (
		rv
	) else (
		let tmp = list_skip m skip_size in
		fetch_down (rv @ [List.hd tmp]) tmp
	) in
	fetch_down [(List.hd m)] m
;;
(* Get the bottom left to top right diagonal *)
let get_diag_up m =
	let skip_size = (!size - 1) in
	let start_list = (list_skip m skip_size) in
	let rec fetch_up rv m =
	if (List.length m = !size) then (
		rv
	) else (
		let tmp = list_skip m skip_size in
		fetch_up (rv @ [List.hd tmp]) tmp
	) in
	fetch_up [(List.hd start_list)] start_list
;;

(* Display the matrix from the flat list *)
let display_matrix m =
	for rn = 0 to (!size -1) do
		print_string("|");
		List.iter (fun i -> print_int(i); print_string("|")) (get_row m rn);
		print_newline()
	done;
	print_endline("-------");
;;

(* Get the row/column containing a specific value *)
let position_of x l =
	let rec position_of_rec x l i =
		if(List.hd l = x) then
			i
		else
			position_of_rec x (List.tl l) (i + 1) in
	position_of_rec x l 0
;;
let row_containing m v =
	get_row m ((position_of v m) / (!size))
;;
let col_containing m v =
	get_col m ((position_of v m) mod (!size))
;;

(* Validation rules *)
let validation = ref [];;

(* Add a validation rule *)
let add_rule name f =
	validation := [(name, f)] @ !validation
;;

(* Run all of the validators *)
let apply_rules m =
	List.iter (fun (name, f) ->
		if (false == f m) then
			raise (Validation_failure name))
		!validation;
;;

(* Validation support *)
let sum l =
	List.fold_left (+) 0 l
;;
let all_even a =
	List.fold_left (fun y x -> y && ((x mod 2) == 0)) true a;
;;
let all_odd a =
	List.fold_left (fun y x -> y && ((x mod 2) == 1)) true a;
;;

(* Debug stuff *)
let snapshot = ref [];;

let display_array a =
	print_string("[");
	List.iter (fun i -> print_int(i); print_string("; ")) a;
	print_endline("]");
;;

(* Generate the sequences *)
let rec gen_seq	thesum a =
	let recurse = (fun () ->
			for i = 1 to (!size * !size) do
				try
					(* Add this to the list only if it isn't already *)
					if (not (List.memq i a)) then
						ignore(gen_seq thesum ([i]@a))
				with Validation_failure x -> ()
			done
		) in
	if (List.length a = (!size * !size)) then (
		snapshot := a;
		apply_rules a;
		display_matrix a;
	) else if (List.length a > 0 && (((List.length a) mod (!size)) == 0)) then (
		(* We always do our calculations from row 0 because we build
		upside-down *)
		if (sum (get_row a 0) = thesum) then
			recurse()
		else
			()
	) else (
		recurse()
	)
;;

(* Debug snapshot display *)
let display_snapshot s =
	print_endline("SNAPSHOT");
	display_matrix !snapshot;
	Gc.print_stat Pervasives.stdout;
	print_endline("/SNAPSHOT");
;;

(** This is the value that the sums will be *)
let sqsum x = x * ( (x * x) + 1) / 2;;

let main() =
	Arg.parse [
		"-s", Arg.Set_int size,
			"Size of the square to calculate (default 4x4)";
		]
		(fun s -> ()) "Calculate all magic squares.";
	print_endline("Calculating magic squares of size " ^ (string_of_int !size));
	let thesum = sqsum !size in

	(* Add the diagonal rules *)
	add_rule "diag(down)" (fun m -> sum (get_diag_down m) = thesum);
	add_rule "diag(up)" (fun m -> sum (get_diag_up m) = thesum);
	(* Add all of the rules for row and column sums *)
	for i = 0 to (!size - 1) do
		add_rule ("sum(r" ^ (string_of_int i) ^ ")")
			(fun m -> sum (get_row m i) = thesum);
		add_rule ("sum(c" ^ (string_of_int i) ^ ")")
			(fun m -> sum (get_col m i) = thesum);
	done;

	(* Set the signal so control-backslash will show where we are *)
	Sys.set_signal Sys.sigquit (Sys.Signal_handle(display_snapshot));

	(* Do the work *)
	ignore(gen_seq thesum [])
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;

