(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 4310B523-2C79-11D8-B89E-000393CB0F1E
 *)

open Cdb;;

let myadd c a b =
	Printf.eprintf "Writing %s = %s\n" a b;
	add c a b
;;

(** test app to create ``test.cdb'' and put some stuff in it *)
let main() =
	let c = open_out "test.cdb" in
	myadd c "a" "1";
	myadd c "b" "2";
	myadd c "c" "3";
	myadd c "c" "4";
	add c "1,473114043983" "1,473114043983";
	myadd c "dustin" "We're number one!";
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
	print_endline("*** Failed match ***");
	(
		try
			print_endline("*** Searching x ***");
			print_endline("ERROR:  " ^ find cdf "x");
		with Stream.Failure ->
			print_endline("failed as expected")
	);
	close_cdb_in cdf;
;;


(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;
