(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: C15A67EC-9CC0-11D8-B65C-000A957659CC
 *)

let main() =
	let ht = Hashtbl.create 500000 in
	Fileutils.iter_file_lines (fun l ->
		let parts = Extstring.split l ' ' 2 in
		Hashtbl.replace ht (List.nth parts 0) (List.nth parts 1)) Sys.argv.(1);
	Fileutils.iter_lines (fun l ->
		let ser = String.sub l 0 10 in
		print_endline( (Hashtbl.find ht ser) ^ " " ^ l)) stdin;
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

