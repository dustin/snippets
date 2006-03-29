(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: D0521BA9-BA6B-11D8-85A6-000393CFE6B8
 *)

open Mfgcdb

exception Duplicate of string
exception InvalidRevMap of (string * int)
exception MissingRevMap of int;;
exception TooManyErrors of string

let maxMissingRecords = ref 0
let maxNewRecords = ref 200000
let maxDifferentRecords = ref 0

let missingRecords = ref 0
let newRecords = ref 0
let differentRecords = ref 0

let verbose = ref false

let usage() =
	prerr_endline("Usage:  " ^ Sys.argv.(0) ^ " [options] oldcdb newcdb");
	prerr_endline("Try " ^ Sys.argv.(0) ^ " -help for more information.");
	exit 1

(* Add up missing and different values *)
let checkMissingAndDiff oldcdbFile newcdbFile =
	let newcdb = Cdb.open_cdb_in newcdbFile in
	Cdb.iter (fun k oldv ->
			try
				let newv = (Cdb.find newcdb k) in
				if newv <> oldv then (
					if (!verbose) then (
						Printf.printf "differing %s (n, o) (%s != %s)\n" k
							newv oldv;
					) else (
						Printf.printf "differing %s\n" k;
					);
					differentRecords := !differentRecords + 1;
					if !differentRecords > !maxDifferentRecords then (
						raise (TooManyErrors "maxDifferentRecords")
					)
				)
			with Not_found ->
				Printf.printf "missing %s\n" k;
				missingRecords := !missingRecords + 1;
				if !missingRecords > !maxMissingRecords then (
					raise (TooManyErrors "maxMissingRecords")
				)
		) oldcdbFile;
	Cdb.close_cdb_in newcdb

(* Add up new records *)
let checkNew oldcdbFile newcdbFile =
	let oldcdb = Cdb.open_cdb_in oldcdbFile in
	Cdb.iter (fun k newv ->
			try
				ignore(Cdb.find oldcdb k);
			with Not_found ->
				newRecords := !newRecords + 1;
				if !newRecords > !maxNewRecords then (
					raise (TooManyErrors "maxNewRecords")
				)
		) newcdbFile;
	Cdb.close_cdb_in oldcdb

let checkRevMap cdbFile =
	let cdb = Cdb.open_cdb_in cdbFile in
	let mfgdb = Mfgcdb.open_mfg_db cdbFile in
	Mfgcdb.iter (fun r ->
			try
				let rsn = Cdb.find cdb ("r" ^ (string_of_int r.gateway_key)) in
				if rsn <> r.sn then
					raise (InvalidRevMap (r.sn, r.gateway_key))
			with Not_found ->
				raise (MissingRevMap r.gateway_key)
		) mfgdb;
	Mfgcdb.close_mfg_db mfgdb;
	Cdb.close_cdb_in cdb

(* Perform all validators *)
let validateCdb oldcdbFile newcdbFile =
	if !verbose then
		Printf.printf "Checking for missing or different records\n%!";
	checkMissingAndDiff oldcdbFile newcdbFile;
	if !verbose then
		Printf.printf "Checking for new records\n%!";
	checkNew oldcdbFile newcdbFile;
	(* These two just abort indicating something went wronger than we expect *)
	if !verbose then
		Printf.printf "Checking reverse mappings\n%!";
	checkRevMap newcdbFile

let main () =
	let rest = ref [] in
	Arg.parse [
		("--max-missing", Arg.Set_int(maxMissingRecords),
			"Maximum acceptable number of missing records in the new cdb");
		("--max-new", Arg.Set_int(maxNewRecords),
			"Maximum acceptable number of new records in the new cdb (200k)");
		("--max-different", Arg.Set_int(maxDifferentRecords),
			"Maximum acceptable number of different records in the new cdb");
		("-v", Arg.Set(verbose),
			"Turn on verbosity");
		] (fun s -> rest := !rest @ [s])
		"Validate a new cdb against a known good one";
	if List.length(!rest) != 2 then usage();
	validateCdb (List.nth !rest 0) (List.nth !rest 1);
	Printf.printf "Diff\t%d\n" !differentRecords;
	Printf.printf "Missing\t%d\n" !missingRecords;
	Printf.printf "New\t%d\n" !newRecords
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
