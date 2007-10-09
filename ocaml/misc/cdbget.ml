(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *)

let usage () =
	prerr_endline("Usage:  " ^ Sys.argv.(0) ^ " cdbfile key [key...]");
	exit 1

let main() =
	try
		let cdb = Cdb.open_cdb_in Sys.argv.(1) in
		let firstfile = Sys.argv.(2) in (* Just for arg checking *)
		List.iter (fun k ->
			try
				let v = (Cdb.find cdb k) in
				Printf.printf "``%s'' = ``%s''\n" k v;
			with Not_found ->
				Printf.printf "``%s'' = NOT FOUND\n" k;
		) (Extlist.nthtail (Array.to_list Sys.argv) 2);
		Cdb.close_cdb_in cdb
	with Invalid_argument("index out of bounds") ->
		usage()
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
