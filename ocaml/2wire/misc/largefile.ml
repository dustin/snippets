(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 761268AC-4600-11D8-B27E-000393CFE6B8
 *)

(** Make a large file. *)

open Unix;;

let main() =
	let f = openfile (Sys.argv.(1)) [O_CREAT;O_RDWR] 0o666 in
	let s = Int64.of_string(Sys.argv.(2)) in
	ignore(LargeFile.lseek f (Int64.pred s) SEEK_SET);
	ignore(Unix.write f "x" 0 1)
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;
