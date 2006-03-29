(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: F8A783C0-A057-11D8-BCC6-000393CFE6B8
 *)

open Mfgcdb

let usage () =
	Printf.eprintf "Usage:  %s cdbfile [keylistfile]\n" Sys.argv.(0);
	exit 1

(** Print a record *)
let record_printer r =
	Printf.printf "%d|%s\n" r.gateway_key r.sn

(** Dump keys found in a file containing one pl,sn per line *)
let dumpSomeKeys from keylist =
	let db = Mfgcdb.open_mfg_db from in
	Fileutils.iter_file_lines (fun l ->
		try
			record_printer (Mfgcdb.lookup db l)
		with x ->
			Printf.eprintf "Could not look up ``%s'':  %s\n"
				l (Printexc.to_string x)
		) keylist;
	Mfgcdb.close_mfg_db db

(** Dump all keys from the cdb file *)
let dumpAllKeys from =
	let db = Mfgcdb.open_mfg_db from in
	Mfgcdb.iter record_printer db;
	Mfgcdb.close_mfg_db db

let main () =
	match Array.length Sys.argv with
		  2 -> dumpAllKeys Sys.argv.(1)
		| 3 -> dumpSomeKeys Sys.argv.(1) Sys.argv.(2)
		| _ -> usage()
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
