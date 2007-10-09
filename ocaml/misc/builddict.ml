(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

(* Build a dictionary cdb from a word list.  Keys will be the words
   (lowecased), values will be empty. *)

let main() =
	let cdb = Cdb.open_out Sys.argv.(1) in
	Fileutils.iter_file_lines (fun l -> Cdb.add cdb l "") Sys.argv.(2);
	Cdb.close_cdb_out cdb

let xmain() =
	Fileutils.iter_file_lines (fun l ->
			print_endline("+" ^ (string_of_int (String.length l))
				^ ",0:" ^ l ^ "->"))
		Sys.argv.(1);
	print_newline()
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
