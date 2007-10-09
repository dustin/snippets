(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *)

let usage () =
	prerr_endline("Usage:  " ^ Sys.argv.(0) ^ " destfile srcfile [srcfile...]");
	exit 1

exception Invalid_cdb

(* This function gets the meta_inf as described in all of the srcs.
   An exception will be thrown if any source is missing a meta_inf, or if it
   disagrees with any other meta_inf found. *)
let getMetaInf = function
	hd::tl -> (
		try
			let firstcdb = Cdb.open_cdb_in hd in
			let first_mf = Cdb.find firstcdb "meta_inf" in
			Cdb.close_cdb_in firstcdb;

			List.iter (fun x ->
				try
					let cdb = Cdb.open_cdb_in x in
					let mf = Cdb.find cdb "meta_inf" in

					if mf <> first_mf then (
						Printf.eprintf "Incompatible version in %s\n" x;
						raise Invalid_cdb
					);

					Cdb.close_cdb_in cdb
				with Not_found ->
					Printf.eprintf "No meta_inf in %s\n" x
				) tl;
			first_mf
		with Not_found ->
			Printf.eprintf "No meta_inf in %s\n" hd;
			raise Invalid_cdb
		)
	| [] -> usage ()

let runWithDest = function
	dest::srcs ->
		if srcs = [] then usage();
		let destcdb = Cdb.open_out dest in
		Cdb.add destcdb "meta_inf" (getMetaInf srcs);
		(* Copy all values from all supplied cdbs into the new dest *)
		List.iter (Cdb.iter (fun k v ->
			if k <> "meta_inf" then
				Cdb.add destcdb k v
			)) srcs;
		Cdb.close_cdb_out destcdb
	| [] -> usage ()

let main() =
	match (Array.to_list Sys.argv) with
		[] -> usage()
		| name::args -> runWithDest args
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
