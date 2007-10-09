(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *)

let usage() =
	Printf.eprintf "Usage:  %s destcdb sourcefile\n" Sys.argv.(0);
	exit(1)

let main() =
	try
		let destcdb = Cdb.open_out Sys.argv.(1) in
		Fileutils.iter_file_lines (fun l ->
				let parts = List.nth (Extstring.split_all l '|' 2) in
				(* Map in both directions *)
				Cdb.add destcdb (parts 0) (parts 1);
				Cdb.add destcdb (parts 1) (parts 0);
			) Sys.argv.(2);
		Cdb.close_cdb_out destcdb
	with Invalid_argument("out-of-bound array or string access") ->
		usage()
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
