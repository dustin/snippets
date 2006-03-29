(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 819239A2-1093-4778-BB52-520DD58AB70D
 *)

open Mfgcdb

let missing = ref 0
let wrong = ref 0

let main () =
	(* Grab a cdb *)
	let db = Mfgcdb.open_mfg_db Sys.argv.(2) in
	(* Flip through all records in the text file and look for matching records
		in the cdb *)
	Fileutils.iter_file_lines (fun l ->
			let parts = List.nth (Extstring.split_all l '|' 3) in
			let id = int_of_string(parts 0) and sn = parts 1 in
			try
				let r = Mfgcdb.lookup db sn in
				if r.sn != (parts 1) then (
					wrong := !wrong + 1;
					Printf.printf "Invalid record.  Expected %s %d got %s %d\n"
						(parts 1) id r.sn r.gateway_key
				)
			with Mfgcdb.No_such_gateway(x) ->
				missing := !missing + 1;
				Printf.printf "No such device %s\n" x
		) Sys.argv.(1);

	Mfgcdb.close_mfg_db db;

	if !missing != 0 then (
		failwith (Printf.sprintf "%d records missing" !missing)
	);
	if !wrong != 0 then (
		failwith (Printf.sprintf "%d records incorrect" !wrong)
	)
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
