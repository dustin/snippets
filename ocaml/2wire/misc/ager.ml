(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: C084E75E-426F-11D8-BC16-000A957659CC
 *)

open Unix;;

(** Age files into appropriate buckets. *)

let age_of s =
	int_of_float ((Unix.time() -. ((Unix.stat s).st_ctime)) /. 86400.0)
;;

let bucket_select a =
	if a < 30 then "."
	else if a < 90 then "90"
	else if a < 180 then "180"
	else "ancient"
;;

let move_to f b =
	(
	try
		Unix.access b [Unix.F_OK]
	with Unix_error(_,_,_) ->
		Unix.mkdir b 0o777;
	);
	let bn = Filename.basename f in
	if not (Sys.file_exists (b ^ "/" ^ bn)) then (
		Printf.printf "Moving %s to %s\n" f b;
		Sys.rename f (b ^ "/" ^ bn);
	)
;;

let move f =
	let b = bucket_select (age_of f) in
	move_to f b
;;

let main() =
	let files = List.filter (fun f -> not (Fileutils.isdir f))
		(List.tl (Array.to_list Sys.argv)) in
	List.iter move files
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;
