(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 238E5114-E446-11D8-A2D2-00039359E8C6
 *
 * Look for lines in a file that match prefixes given in the limitfile.
 *)

module StringSet = Set.Make(String);;

let usage () =
	prerr_endline("Usage:  " ^ Sys.argv.(0) ^ " limitfile sourcefile");
	exit 1
;;

let check_match s a b =
	if (Extstring.begins_with s a) || (Extstring.begins_with s b) then
		Printf.printf "%s\n" s
;;

let main() =
	try
		let limit = Fileutils.fold_file_lines StringSet.add
			StringSet.empty Sys.argv.(1) in
		(* Look for lines beginning with stuff that made it into the match *)
		Fileutils.iter_file_lines (fun l ->
			let (lt, b, gt) = StringSet.split l limit in
				(*
				Printf.printf "\n%s\nSplit to %s - %s and %s - %s\n" l
					(StringSet.min_elt lt) (StringSet.max_elt lt)
					(StringSet.min_elt gt) (StringSet.max_elt gt);
				*)
				try
					check_match l (StringSet.max_elt lt) (StringSet.min_elt gt)
				with Not_found -> ()
			) Sys.argv.(2);
	with Invalid_argument("out-of-bound array or string access") ->
		usage()
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

