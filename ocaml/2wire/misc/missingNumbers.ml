(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: D00ACB34-20B0-11D8-84F5-00039359E8C6
 *)

(*
 * Specification:
 *
 * Sum of rows (14,15,16)
 * Sum of columns (14,15,16)
 * Row 0 is all even.
 * Row 1 is all odd.
 * 4 and 6 are in the same column
 * 1 and 9 are in the same row
 * 6 is not in the same row as 4
 *)

exception Validation_failure of string;;

(* Translate the flat array to rows and columns *)
let get_row m x =
	let base = x * 3 in
	[List.nth m (base); List.nth m (base + 1); List.nth m (base + 2)]
;;
let get_col m x =
	[List.nth m x; List.nth m (x+3); List.nth m (x+6)]
;;

(* Display the 3x3 matrix from the flat list *)
let display_matrix m =
	List.iter (fun l ->
				print_string("|");
				List.iter (fun i -> print_int(i); print_string("|")) l;
				print_newline()) [get_row m 0; get_row m 1; get_row m 2;];
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
	get_row m ((position_of v m) / 3)
;;
let col_containing m v =
	get_col m ((position_of v m) mod 3)
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

(* Generate the sequences *)
let rec gen_seq	a =
	if (List.length a == 9) then
	begin
		apply_rules a;
		display_matrix a;
	end
	else
		for i = 1 to 9 do
			try
				(* Add this to the list only if it isn't already *)
				if (not (List.memq i a)) then
					ignore(gen_seq ([i]@a))
			with Validation_failure x -> ()
		done;
;;

let main() =
	add_rule "row 0 all even" (fun m -> all_even (get_row m 0));
	add_rule "row 1 all odd" (fun m -> all_odd (get_row m 1));
	add_rule "sum of r0 == 14" (fun m -> sum (get_row m 0) = 14);
	add_rule "sum of r1 == 15" (fun m -> sum (get_row m 1) = 15);
	add_rule "sum of r2 == 16" (fun m -> sum (get_row m 2) = 16);
	add_rule "sum of c1 == 14" (fun m -> sum (get_col m 0) = 14);
	add_rule "sum of c2 == 15" (fun m -> sum (get_col m 1) = 15);
	add_rule "sum of c3 == 16" (fun m -> sum (get_col m 2) = 16);
	add_rule "4 and 6 in same column" (fun m ->
		List.memq 4 (col_containing m 6));
	add_rule "1 and 9 in same row" (fun m ->
		List.memq 1 (row_containing m 9));

	ignore(gen_seq [])
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

