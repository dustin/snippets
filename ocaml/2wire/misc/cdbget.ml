(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: DBE4BA87-A5E1-11D8-A0DF-000393CFE6B8
 *)

let usage () =
	prerr_endline("Usage:  " ^ Sys.argv.(0) ^ " cdbfile key");
	exit 1
;;

let main() =
	try
		let cdb = Cdb.open_cdb_in Sys.argv.(1) in
		Printf.printf "``%s''\n" (Cdb.find cdb Sys.argv.(2));
		Cdb.close_cdb_in cdb
	with Invalid_argument("out-of-bound array or string access") ->
		usage()
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

