(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: CDBD407A-4148-11D8-95F3-000393CFE6B8
 *)

open Unix;;

let resolve h =
	try
		string_of_inet_addr (gethostbyname h).h_addr_list.(0)
	with Not_found -> "Not_found"
;;

let main() =
	Fileutils.iter_lines (fun l ->
		print_endline(l ^ " " ^ (resolve l)))
		Pervasives.stdin
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;
