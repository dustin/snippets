(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 90CC9C89-194B-11D8-8F8B-000393CFE6B8
 *)

let index_one fn =
	print_endline("Indexing " ^ fn);
	(* Get the CDB *)
	let cdb = Cdb.open_out fn in
	Fileutils.iter_file_lines (fun l ->
		let a = Extstring.split l '\t' 2 in
		try
			Cdb.add cdb (List.hd a) l
		with
			| Failure("hd") ->
				print_endline("Format problem:  " ^ l)) fn;
	Cdb.close_cdb_out cdb;
;;

let main() =
	Arg.parse [ ]
		index_one
		"Index the given list of files.";
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

