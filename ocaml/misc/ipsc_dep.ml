(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 4F4D4DCC-AA2F-11D8-9E24-000A957659CC
 *
 * See http://ipsc.ksp.sk/sample.php?arg_contest=ipsc2004
 *)

module StringSet = Set.Make(String);;

(* Recursively try to install all packages until we can't install anymore *)
let rec resolve ht seen =
	let installed = Hashtbl.fold (fun k v acc ->
			if ((not (StringSet.mem k seen))
				&& StringSet.is_empty (StringSet.diff v seen)) then (

				StringSet.add k acc
			) else (
				acc
			)
		)
		ht StringSet.empty in
	(* If we installed anything, try again *)
	if (not (StringSet.is_empty installed)) then (
		resolve ht (StringSet.union installed seen)
	) else (
		StringSet.union installed seen
	)
;;

(* Make a string set from a list *)
let setof l =
	List.fold_left (fun acc x -> StringSet.add x acc) StringSet.empty l
;;

(* Load the data and send it off for processing *)
let main() =
	let ht = Hashtbl.create 1 in
	Fileutils.iter_file_lines (fun l ->
			match Extstring.split l ' ' max_int with
			  [] -> failwith("Invalid line:  " ^ l)
			| pkg::deps -> Hashtbl.replace ht pkg
				(setof (List.filter ((<>) "0") deps));
		) Sys.argv.(1);
	let installed = resolve ht StringSet.empty in
	Printf.printf "%d\n" (StringSet.cardinal installed)
	(* StringSet.iter (fun l -> Printf.printf "Installed %s\n" l) installed; *)
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

