(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: EB4966BA-A5EA-11D8-9EB0-000393CFE6B8
 *)

let usage () =
	prerr_endline("Usage:  " ^ Sys.argv.(0) ^ " cdbfile");
	exit 1
;;

let rec countStream s n =
	try
		Stream.empty s;
		n
	with Stream.Failure ->
		Stream.next s;
		countStream s (n + 1)
;;

let main() =
	let found = ref 0 in
	let different = ref 0 in
	let notfound = ref 0 in
	try
		let cdb = Cdb.open_cdb_in Sys.argv.(1) in
		Cdb.iter (fun k v ->
					match countStream (Cdb.get_matches cdb k) 0 with
						  0 ->
							Printf.eprintf "No match at %s\n" k;
							notfound := succ !notfound
						| 1 -> found := succ !found
						| _ ->	found := succ !found;
								different := succ !different
				) Sys.argv.(1);
		Cdb.close_cdb_in cdb;
		Printf.printf "found:\t\t%d\nnot found:\t%d\ndifferent\t%d\n"
			!found !notfound !different
	with Invalid_argument("index out of bounds") ->
		usage()
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

