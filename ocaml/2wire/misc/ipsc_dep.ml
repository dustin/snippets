(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 4F4D4DCC-AA2F-11D8-9E24-000A957659CC
 *
 * See http://ipsc.ksp.sk/sample.php?arg_contest=ipsc2004
 *)

module StringSet = Set.Make(String);;
module StringMap = Map.Make(String);;

(* verbose output flag *)
let verbose = ref false;;

(* Recursively try to install all packages until we can't install anymore *)
let rec resolve stuff seen rv =
	let installed = StringMap.fold (fun k v acc ->
			if ((not (StringSet.mem k seen))
				&& StringSet.is_empty (StringSet.diff v seen)) then (

				StringSet.add k acc
			) else (
				acc
			)
		)
		stuff StringSet.empty in
	(* If we installed anything, try again *)
	if (not (StringSet.is_empty installed)) then (
		resolve (StringSet.fold StringMap.remove installed stuff)
			(StringSet.union installed seen) (installed::rv)
	) else (
		List.rev rv
	)
;;

(* Make a string set from a list *)
let setof l =
	List.fold_left (fun acc x -> StringSet.add x acc) StringSet.empty l
;;

let run filename =
	let stuff = Fileutils.fold_file_lines (fun l acc ->
			match Extstring.split l ' ' max_int with
			  [] -> failwith("Invalid line:  " ^ l)
			| pkg::deps ->
				StringMap.add pkg (setof (List.filter ((<>) "0") deps)) acc
		) StringMap.empty filename in
	let installed = resolve stuff StringSet.empty [] in
	let installed_count = List.fold_left (fun acc i ->
											acc + (StringSet.cardinal i))
										0 installed in
	Printf.printf "%d\n" installed_count;
	(* The following actually shows the steps and what can be installed *)
	if !verbose then (
		Extlist.iteri (fun i l ->
				Printf.printf "Step %d installed %d\n"
					(succ i) (StringSet.cardinal l);
				StringSet.iter (fun el -> Printf.printf "\t%s\n" el) l
			) installed
	)
;;

(* Load the data and send it off for processing *)
let main() =
	Arg.parse ["-v", Arg.Unit(fun _ -> verbose := true), "Show verbose output"]
		run "Process dependency file"
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

