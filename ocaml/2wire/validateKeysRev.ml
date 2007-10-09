(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *)

open Mfgcdb

exception ExcessiveBadness of int

let badCount = ref 0

(* Get the productline,serialnumber from a serial number (that may be prefixed
   with a product line already).  If it's not, prefix with 1. *)
let get_sn s =
	if Extstring.str_index_of_one s [','] 0 = -1 then (
		"1," ^ s
	) else (
		s
	)

(* Do a lookup function and its error handling for the given line *)
let doLookupFun db l f =
	try
		f()
	with
		  Mfgcdb.No_such_gateway sn ->
			badCount := !badCount + 1;
			Printf.eprintf "No record found for %s (%s)\n" l sn;
		| x ->
			badCount := !badCount + 1;
			Printf.eprintf "Unexpected error (%s) on %s\n"
				(Printexc.to_string x) l

(* Check sn to key mappings *)
let checkSnToKey db l sn key =
	doLookupFun db l (fun () ->
		let record = Mfgcdb.lookup db sn in
		if record.gateway_key <> int_of_string key then (
			badCount := !badCount + 1;
			Printf.eprintf "sn -> key mismatch.  Got %s -> %d for line %s\n"
				record.sn record.gateway_key l
		)
	)

let go () =
	let db = Mfgcdb.open_mfg_db Sys.argv.(2) in

	(* Flip through the file and validate all of the records *)
	Fileutils.iter_file_lines (fun lin ->
		let l = Extstring.strip lin in
		(* parts now has the chunks from the dump *)
		let parts = List.nth (Extstring.split_all l '|' 3) in
		let key = Extstring.strip (parts 0) in
		let sn = get_sn (Extstring.strip (parts 1)) in
			checkSnToKey db l sn key;
		) Sys.argv.(1);
	Mfgcdb.close_mfg_db db;

	if !badCount <> 0 then (
		raise (ExcessiveBadness !badCount)
	)

let main () =
	try
		go()
	with Invalid_argument("index out of bounds") ->
		Printf.eprintf "Usage:  %s cpedump mfg.cdb\n" Sys.argv.(0);
		exit 1
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
