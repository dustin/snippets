(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 4310B523-2C79-11D8-B89E-000393CB0F1E
 *)

open Cdb;;

(** test app to create ``test.cdb'' and put some stuff in it *)
let main() =
	let c = open_out "test.cdb" in
	add c "a" "1";
	add c "b" "2";
	add c "c" "3";
	add c "c" "4";
	add c "dustin" "We're number one!";
	close_cdb_out c;
	iter (fun k v -> print_endline(k ^ " -> " ^ v)) "test.cdb";
	print_endline("*** Searching a ***");
	let cdf = open_cdb_in "test.cdb" in
	print_endline(find cdf "a");
	print_endline("*** Searching c ***");
	print_endline(find cdf "c");
	print_endline("*** Stream searching c ***");
	let str = get_matches cdf "c" in
	let str2 = get_matches cdf "c" in
	print_endline(Stream.next str);
	print_endline(Stream.next str2);
	print_endline(Stream.next str);
	print_endline(Stream.next str2);
	(
	try
		print_endline("Testing stream exhaustion failure");
		print_endline(Stream.next str);
		print_endline("!!! Expected failure.");
	with Stream.Failure -> print_endline("failed as expected")
	);
	print_endline("*** Stream.iter ***");
	Stream.iter print_endline (get_matches cdf "c");
	close_cdb_in cdf;
;;


(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;
