(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 38077BFA-9FB7-11D8-B01E-000393CFE6B8
 *)

let usage () =
	prerr_endline("Usage:  " ^ Sys.argv.(0) ^ " destfile srcfile [srcfile...]");
	exit 1
;;

let runWithDest = function
	dest::srcs ->
		if srcs = [] then usage();
		let destcdb = Cdb.open_out dest in
		(* Copy all values from all supplied cdbs into the new dest *)
		List.iter (Cdb.iter (Cdb.add destcdb)) srcs;
		Cdb.close_cdb_out destcdb
	| [] -> usage ()
;;

let main() =
	match (Array.to_list Sys.argv) with
		[] -> usage()
		| name::args -> runWithDest args
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

