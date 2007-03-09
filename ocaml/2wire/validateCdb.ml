(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: D0521BA9-BA6B-11D8-85A6-000393CFE6B8
 *)

open Mfgcdb

exception Duplicate of string
exception InvalidRevMap of (string * int)
exception MissingRevMap of int
exception TooManyErrors of string

(* Paths to CDBs *)
type cdbpaths = {
	revcdb_path: string;
	forwardcdb_path: string array;
}

type multicdb = {
	revcdb: Cdb.cdb_file;
	forwardcdb: Cdb.cdb_file array;
}

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

let iter_many_cdbs f cdbs =
	Cdb.iter f cdbs.revcdb_path;
	Array.iter (Cdb.iter f) cdbs.forwardcdb_path

let get_a_record k stuff =
	if (Extstring.begins_with k "r") then
		Cdb.find stuff.revcdb k
	else
		Cdb.find (Mfgcdb.get_cdb_for_key_from_array stuff.forwardcdb k) k

let close_cdbs stuff =
	Cdb.close_cdb_in stuff.revcdb;
	Array.iter Cdb.close_cdb_in stuff.forwardcdb

let open_cdbs stuff =
	{
		revcdb=Cdb.open_cdb_in stuff.revcdb_path;
		forwardcdb=Array.map Cdb.open_cdb_in stuff.forwardcdb_path;
	}

let get_cdb_list path =
	if (Fileutils.isdir path) then
		{
			revcdb_path=path ^ "/reverse.cdb";
			forwardcdb_path=Array.init (Mfgcdb.count_cdbs path)
				(Printf.sprintf "%s/forward.%d.cdb" path);
		}
	else
		{
			revcdb_path=path;
			forwardcdb_path=Array.make 1 path;
		}

(* Add up missing and different values *)
let checkMissingAndDiff oldcdbFile newcdbFile =
	let newcdb = open_cdbs newcdbFile in
	iter_many_cdbs (fun k oldv ->
			try
				let newv = (get_a_record k newcdb) in
				if newv <> oldv then (
					if (!verbose) then (
						Printf.printf "differing %s (n, o) (%s != %s)\n%!" k
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
	close_cdbs newcdb

(* Add up new records *)
let checkNew oldcdbFile newcdbFile =
	let oldcdb = open_cdbs oldcdbFile in
	iter_many_cdbs (fun k newv ->
			try
				ignore(get_a_record k oldcdb);
			with Not_found ->
				newRecords := !newRecords + 1;
				if !newRecords > !maxNewRecords then (
					raise (TooManyErrors "maxNewRecords")
				)
		) newcdbFile;
	close_cdbs oldcdb

let checkRevMap cdbFile =
	let cdb = Cdb.open_cdb_in (get_cdb_list cdbFile).revcdb_path in
	let mfgdb = Mfgcdb.open_mfg_db cdbFile in
	Mfgcdb.iter (fun r ->
			try
				let rsn =
					Cdb.find cdb ("r" ^ (string_of_int r.gateway_key)) in
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
	checkNew oldcdbFile newcdbFile

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

	validateCdb (get_cdb_list (List.hd !rest))
		(get_cdb_list (List.nth !rest 1));
	(* These two just abort indicating something went wronger than we expect *)
	if !verbose then
		Printf.printf "Checking reverse mappings\n%!";
	checkRevMap (List.nth !rest 1);

	Printf.printf "Diff\t%d\n" !differentRecords;
	Printf.printf "Missing\t%d\n" !missingRecords;
	Printf.printf "New\t%d\n" !newRecords
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
