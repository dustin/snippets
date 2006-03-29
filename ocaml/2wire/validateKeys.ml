(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 7F64B092-B8DD-11D8-A1B6-000393CFE6B8
 *)

open Mfgcdb

let validateCdb ht from =
	let db = Mfgcdb.open_mfg_db from in
	Mfgcdb.iter (fun r ->
			try
				let key = Hashtbl.find ht r.sn in
				if key != r.gateway_key then (
					Printf.printf "Invalid key for %s (wanted %d, got %d)\n"
						r.sn key r.gateway_key
				)
			with Not_found ->
				Printf.printf "*** %s's key (%d) was generated\n"
					r.sn r.gateway_key
		) db;
	Mfgcdb.close_mfg_db db

let main () =
	let ht = Hashtbl.create 1 in
	(* Load the reserved keys *)
	Fileutils.iter_file_lines (fun l ->
			try
				let parts = List.nth (Extstring.split_all l '|' 3) in
				let id = int_of_string(parts 0) and sn = parts 1 in
				Hashtbl.replace ht sn id
			with x ->
				Printf.eprintf "Reserved malformed line warning ``%s'':  %s\n"
					l (Printexc.to_string x)
		) Sys.argv.(2);
	(* Iterate the cdb and validate the keys are correct according to the
		reserved file *)
	validateCdb ht Sys.argv.(1)
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
