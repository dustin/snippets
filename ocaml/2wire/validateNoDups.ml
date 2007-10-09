(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *)

open Mfgcdb

exception Duplicates

let duplicates = ref 0

let usage() =
	prerr_endline("Usage:  " ^ Sys.argv.(0) ^ " cdb");
	exit 1

(* Count how many times we can successfully call Stream.next on a stream *)
let count_stream s =
	let rec loop n = (
	try
		Stream.next s;
		loop (n + 1)
	with Stream.Failure ->
		n
	) in loop 0

(* Check for duplicates (more than one value per key) *)
let checkForDups cdbFile =
	let cdb = Cdb.open_cdb_in cdbFile in
	Cdb.iter (fun k v ->
		let nmatches = (count_stream (Cdb.get_matches cdb k)) in
		if nmatches > 1 then (
			duplicates := !duplicates + 1;
			Printf.printf "Duplicate:  %s\n" k
		);
		assert (nmatches <> 0);
		) cdbFile;
	Cdb.close_cdb_in cdb

let main () =
	if Array.length(Sys.argv) != 2 then usage();
	checkForDups Sys.argv.(1);
	if !duplicates <> 0 then
		raise Duplicates
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
