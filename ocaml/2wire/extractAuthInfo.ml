(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 222185e2-7d45-4a8e-b6cb-6422c569c8c3
 *)

open MfgCdb;;

let main() =
	let cdbFile = Sys.argv.(1) in
	let db = MfgCdb.open_mfg_db cdbFile in
	MfgCdb.iter (fun m ->
		Printf.printf "%s\t%s\t%s\n" m.sn m.id_string
			(Base64.encode_string
				(Digest.string (m.id_string ^ m.sn)))
		) db;
	MfgCdb.close_mfg_db db;
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;

