(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 90CC9C89-194B-11D8-8F8B-000393CFE6B8
 *)

let index_one fn =
	print_endline("Indexing " ^ fn);
	(* Get the DBM *)
	let db = Dbm.opendbm fn [Dbm.Dbm_create;Dbm.Dbm_rdwr] 0o644 in
	Fileutils.iter_file_lines (fun l ->
		let a = Extstring.split l '\t' 2 in
		try
			Dbm.add db (List.hd a) l
		with
			Dbm.Dbm_error("Entry already exists") ->
				print_endline("Duplicate: " ^ l)
			| Failure("hd") ->
				print_endline("Format problem:  " ^ l)) fn;
	Dbm.close db;
;;

let main() =
	Arg.parse [ ]
		index_one
		"Index the given list of files.";
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

