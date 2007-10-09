(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *)

let lookup ht s =
	try Hashtbl.find ht s
	with Not_found -> "Not_found"

let main() =
	let ht = Hashtbl.create 500000 in
	Fileutils.iter_file_lines (fun l ->
		let parts = Extstring.split l ' ' 2 in
		Hashtbl.replace ht (List.nth parts 0) (List.nth parts 1)) Sys.argv.(1);
	Fileutils.iter_lines (fun l ->
		let ser = String.sub l 0 12 in
		print_endline( (lookup ht ser) ^ " " ^ l)) stdin
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
