(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 965553-138-4071-80-1766087
 *)

let nameMapping =
	let ht = Hashtbl.create 1 in
	let m = Hashtbl.replace ht in
	m "HEARTBEAT_SCHEDULED" "HB_SCHEDULED";
	m "HEARTBEAT_PERIODIC" "HB_PERIODIC";
	m "HEARTBEAT_NEWIP" "HB_NEWIP";
	m "HEARTBEAT_BOOT" "HB_BOOT";
	m "PACKAGESET_MODIFY_STATUS" "PKGSET_MOD";
	m "PACKAGE_MODIFY_STATUS" "PKG_MOD";
	ht
;;

let lookupName n =
	try
		Hashtbl.find nameMapping n
	with Not_found ->
		n
;;

let main() =
	let fn = Sys.argv.(1) in
	Printf.eprintf "Updating %s\n" fn;
	flush stderr;
	Fileutils.iter_lines (fun l ->
			try
				let a = Extstring.split l ' ' 99999 in
				let ts = Int32.of_float(float_of_string (List.hd a)) in
				(* Create an array of keys and an array of values *)
				let keys,vals = List.fold_left (fun (rvk, rvv) pair ->
						let pairsplit = List.nth (Extstring.split pair '=' 2) in
						((lookupName (pairsplit 0)) :: rvk), (pairsplit 1 :: rvv)
					) ([], []) (List.tl a) in
				(* Create the output string *)
				Printf.printf "update %s -t %s %s:%s\n" fn
					(String.concat ":" (List.rev keys))
					(Int32.to_string ts) (String.concat ":" (List.rev vals));
			with x ->
				Printf.eprintf "%s on line:  %s (ignoring)\n"
					(Printexc.to_string x) l;
				flush stderr
		) stdin;
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

