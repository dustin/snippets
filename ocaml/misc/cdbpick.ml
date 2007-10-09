(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *)

module StringSet = Set.Make(String)

let usage () =
	prerr_endline("Usage:  " ^ Sys.argv.(0) ^ " limitfile srcfile destfile");
	exit 1

let main() =
	try
		let limit = Fileutils.fold_file_lines StringSet.add
			StringSet.empty Sys.argv.(1) in
		let destcdb = Cdb.open_out Sys.argv.(3) in
		(* Copy all values from all supplied cdbs into the new dest *)
		Cdb.iter (fun k v ->
					(* Copy the record if the key or value matches *)
					if (StringSet.mem k limit || StringSet.mem v limit) then (
						Cdb.add destcdb k v
					)
				)
				Sys.argv.(2);
		Cdb.close_cdb_out destcdb
	with Invalid_argument("index out of bounds") ->
		usage()
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end

