(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *)

open Unix

let resolve h =
	try
		string_of_inet_addr (gethostbyname h).h_addr_list.(0)
	with Not_found -> "Not_found"

let main() =
	Fileutils.iter_lines (fun l ->
		print_endline(l ^ " " ^ (resolve l)))
		Pervasives.stdin
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
