(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: F8A783C0-A057-11D8-BCC6-000393CFE6B8
 *)

open Mfgcdb;;

let loadMfgKeys from =
	let db = Mfgcdb.open_mfg_db from in
	Mfgcdb.iter (fun r -> Printf.printf "%d|1,%s\n" r.gateway_key r.sn) db;
	Mfgcdb.close_mfg_db db
;;

let main () =
	loadMfgKeys Sys.argv.(1)
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

