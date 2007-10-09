(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

(* for every line of stdin, index by the first element (serial number) *)
let main() =
	let db = Dbm.opendbm "authdb" [Dbm.Dbm_create;Dbm.Dbm_rdwr] 0o644 in
	Fileutils.iter_lines (fun l ->
							Dbm.add db (List.hd (Extstring.split l '\t' 1)) l)
						stdin;
	Dbm.close db
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
