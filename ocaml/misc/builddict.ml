(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 3FE02BAF-343C-11D8-B914-000393CB0F1E
 *)

(* Build a dictionary cdb from a word list.  Keys will be the words
   (lowecased), values will be empty. *)

let xmain() =
	let cdb = Cdb.open_out Sys.argv.(1) in
	Fileutils.iter_file_lines (fun l -> Cdb.add cdb l "") Sys.argv.(2);
	Cdb.close_cdb_out cdb;
;;

let main() =
	Fileutils.iter_file_lines (fun l ->
			print_endline("+" ^ (string_of_int (String.length l))
				^ ",0:" ^ l ^ "->"))
		Sys.argv.(1);
	print_newline();
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;
