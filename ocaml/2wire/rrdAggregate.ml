(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 85307-371-46-31-0012432
 *)

open Statutils

(* All of the columns and their aggregation functions *)
let columns =
	columnMaker (fun _ _ -> avg)
		["HB"; "BOOT"; "KICK"; "XMLRPC"] ["time"; "count"; "start"; "end"]

let parseline l =
	let allcols = Extstring.split l '\t' ((List.length columns) + 1) in
	List.hd allcols, List.map float_of_string (List.tl allcols)

let gotLine cdbs l stat =
	let k, vs = parseline l in
	let morev = List.map (fun x ->
		let nk,nv = parseline (Cdb.find x k) in nv) cdbs in
	let allv = vs :: morev in
	let vals = List.map sum (Extlist.zip allv) in
	print_endline(k ^ "\t" ^
		(String.concat "\t" (List.map string_of_float vals)));
	stat

let process txt cdbs =
	(* Print the first two lines *)
	Fileutils.operate_on_file_in (fun ch ->
			print_endline(input_line ch); (* header *)
			print_endline(input_line ch); (* empty *)
			Fileutils.fold_lines (gotLine cdbs) () ch
		) txt

let main() =
	let arglist = Array.to_list Sys.argv in
	let cmd,tmplist = List.hd arglist, List.tl arglist in
	let first,rest = List.hd tmplist, List.tl tmplist in
	(* Process the first file and all of the cdbs *)
	process
		(first ^ ".txt")
		(List.map (fun f -> Cdb.open_cdb_in (f ^ ".cdb")) rest)
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
