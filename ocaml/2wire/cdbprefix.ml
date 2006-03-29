(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 4584EF25-A2D0-11D8-BEEE-000393CFE6B8
 *)

let usage () =
	prerr_endline("Usage:  " ^ Sys.argv.(0) ^ " prefix srcfile destfile");
	exit 1

let main() =
	(* Validate there are enough args *)
	if Array.length(Sys.argv) < 4 then usage();
	(* Copy the cdb, prepending stuff to all the keys *)
	let prefix = Sys.argv.(1)
		and srcfile = Sys.argv.(2)
		and destfile = Sys.argv.(3) in
	let destcdb = Cdb.open_out destfile in
	Cdb.iter (fun k v ->
			Cdb.add destcdb (if k = "meta_inf" then k else (prefix ^ k)) v)
		srcfile;
	Cdb.close_cdb_out destcdb
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
