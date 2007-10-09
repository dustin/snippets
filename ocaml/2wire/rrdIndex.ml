(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

let index_one fn =
	print_endline("Indexing " ^ fn);
	(* Get the CDB *)
	let cdb = Cdb.open_out (fn ^ ".cdb") in
	Fileutils.iter_file_lines (fun l ->
			let a = Extstring.split l '\t' 2 in
			try
				Cdb.add cdb (List.hd a) l
			with 
				| Failure("hd") ->
					print_endline("Format problem:  " ^ l))
		(fn ^ ".txt");
	Cdb.close_cdb_out cdb

let main() =
	Arg.parse [ ]
		index_one
		"Index the given list of files."
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
