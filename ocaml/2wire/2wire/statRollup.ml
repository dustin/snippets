(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 7AEBB098-9EB2-11D8-AC70-000393CFE6B8
 *)

(* Column type *)
type 'a col_t = {
	name: string;
	avg: ('a list -> 'a);
	aggtype: string;
};;

(* Accumulator type *)
type acct_t = {
	ds: string;
	cols: float list list;
};;

(* Set of strings *)
module StringSet = Set.Make(String);;

(* Get the sum of a list of numbers *)
let sum l = List.fold_left (+.) 0.0 l ;;

(* Get the average of the numbers in a list *)
let simpleAvg l = (sum l) /. float_of_int(List.length l) ;;

(* Get the average of the numbers in a list minus the highest and lowest *)
let avg l =
	(* Sort the list *)
	let sl = List.sort compare l in
	(* Remove the first and last elements *)
	let tl = List.tl (List.rev (List.tl (List.rev sl))) in
	simpleAvg tl
;;

(* All of the columns and their aggregation functions *)
let columns =
	List.concat (List.map (fun t ->
		List.map (fun subt ->
			let cname = t ^ subt in
			{	name = cname;
				avg = if (subt = "time") then avg else simpleAvg;
				aggtype = if (subt = "time") then "avg" else "simpleAvg";
			})
			["time"; "count"; "start"; "end"])
		["HB"; "BOOT"; "KICK"; "XMLRPC"])
;;

(* Display the results *)
let real_display ds cols =
	print_string(ds ^ "\t");
	print_endline(String.concat "\t" (List.map2 (fun col data ->
						string_of_float (86400.0 *. col.avg data))
					columns cols))
;;

(* display if it was a good day *)
let good_display baddays ds cols =
	if (not (StringSet.mem ds baddays)) then
		real_display ds cols
;;

(* Called with each line (this is an accumulator function) *)
let gotLine display l acct =
	try
		let allcols = Extstring.split l '\t' ((List.length columns) + 1) in
		let ds = String.sub l 0 10 in
		let cols = List.map float_of_string (List.tl allcols) in
		if (ds = acct.ds) then (
			{ds = acct.ds; cols = cols :: acct.cols }
		) else (
			display acct.ds (Extlist.zip acct.cols);
			{ds = ds; cols = [cols]}
		)
	with x ->
		prerr_endline("Error on " ^ l);
		raise x
;;

let main() =
	(* Create the display function to avoid the bad dates *)
	let display = good_display (Fileutils.fold_file_lines
			StringSet.add StringSet.empty (Sys.argv.(1))) in
	print_endline("# " ^ input_line stdin); (* header *)
	print_endline("# " ^ input_line stdin); (* empty *)
	(* Process all of the lines *)
	ignore(Fileutils.fold_lines (gotLine display)
		{ds = "11/06/2002"; cols = []} stdin)
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

