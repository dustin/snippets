(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: F31E0A41-A042-11D8-BB34-000393CFE6B8
 *)

open Unix;;

exception Duplicate of string;;

type in_record = {
	in_sn: string;
	in_version: string;
	in_secret: string;
	in_model: string;
	in_password: string;
	in_wirelessid: string;
};;

(* The output fields *)
let cdb_fields = [
	"boxnum"; "modelnum"; "idstring"; "pca"; "productstring";
	"version"; "authcode"; "ssid"; "wirelessid"; "moddate"
];;

let magic_key = "meta_inf";;
let version = "3";;

(* The mapping of seen IDs *)
let seenIds = Hashtbl.create 1;;

(* The meta_inf is the version number followed by a netstring containing the
	list of fields as netstrings *)
let makeMetaInf () =
	(Netstring.encode version) ^ (Netstring.encode
		(List.fold_left (fun l a -> (Netstring.encode a) ^ l) ""
			(* Reverse the fields since we're folding and will be building
				backwards *)
			(List.rev cdb_fields)))
;;

let makeOutRecord product_line sn ht =
	(product_line ^ "," ^ sn), (String.concat "" (List.map
		(fun x -> (Netstring.encode (Hashtbl.find ht x))) cdb_fields))
;;

let rec checkDup n plsn =
	try
		let found = Hashtbl.find seenIds n in
		if (plsn = found ) then (
			raise (Duplicate plsn)
		) else (
			Printf.eprintf "DUPLICATE AT %d (old: %s, new: %s)\n" n found plsn;
			checkDup (n + 1) plsn
		)
	with Not_found ->
		Hashtbl.add seenIds n plsn;
		n
;;

(* Generate a box number from a productline/sn pair *)
let genBoxNum product_line sn =
	let plsn = (product_line ^ "," ^ sn) in
	let dig = String.get (Digest.string plsn) in
	let a,b,c,d =	(Char.code (dig 0)), (Char.code (dig 1)),
					(Char.code (dig 2)), (Char.code (dig 3)) in
	checkDup (abs(a lor (b lsl 8) lor (c lsl 16) lor (d lsl 24))) plsn
;;

(* Like List.nth, but with a default *)
let nthDefault l def n =
	try
		List.nth l n
	with Failure("nth") ->
		def
;;

(* Make a record as a hash table of all known fields *)
let makeRecord modelMap ts l =
	let ht = Hashtbl.create 1 in
	(* Get all of the fields into the hashtable *)
	List.iter (fun i -> Hashtbl.add ht i "") cdb_fields;
	(* Parse the record *)
	let parts = nthDefault (Extstring.split_all l '|' 99) "" in
	let record = {	in_sn=parts 0;
					in_version=parts 1;
					in_secret=parts 2;
					in_model=parts 3;
					in_password=parts 4;
					in_wirelessid=parts 5} in
	try
		let product_line = Hashtbl.find modelMap record.in_model in
		(* Convenience function for updating the hashtable *)
		let htr = Hashtbl.replace ht in
		htr "boxnum" (string_of_int (genBoxNum product_line record.in_sn));
		htr "modelnum" record.in_model;
		htr "pca" record.in_model;
		htr "version" record.in_version;
		htr "authcode" record.in_secret;
		htr "moddate" ts;
		Some (makeOutRecord product_line record.in_sn ht)
	with Not_found ->
		prerr_endline("Unknown model:  " ^ record.in_model);
		None
;;

(* Store a parsed record *)
let storeRecord db modelMap ts l =
	match (makeRecord modelMap ts l) with
	  None -> ()
	| Some(k,v) ->
		print_endline(k ^ " -- " ^ v);
		Cdb.add db k v
;;

(* Parse a model map line *)
let parseModelMap ht l =
	let parts = List.nth (Extstring.split_all l '|' 3) in
	Hashtbl.add ht (parts 0) (parts 1)
;;

let loadMfgKeys from =
	Fileutils.iter_file_lines (fun l ->
			let parts = List.nth (Extstring.split_all l '|' 3) in
			Hashtbl.add seenIds (int_of_string (parts 0)) (parts 1)
		) from
;;

let processFile destcdb modelMap filename =
	let tm = Unix.gmtime (Unix.stat filename).st_mtime in
	let ts = Printf.sprintf "%04d%02d%02dT%02d%02d%02d"
			(tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
			tm.tm_hour tm.tm_min tm.tm_sec in
	Printf.eprintf "File timestamp is %s\n" ts;
	Fileutils.iter_file_lines (storeRecord destcdb modelMap ts) filename

;;

let main () =
	let destcdb = Cdb.open_out Sys.argv.(1) in
	loadMfgKeys Sys.argv.(2);
	let modelMap = Hashtbl.create 1 in
	print_endline(makeMetaInf());
	Fileutils.iter_file_lines (parseModelMap modelMap) Sys.argv.(3);
	processFile destcdb modelMap Sys.argv.(4);
	Cdb.close_cdb_out destcdb
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

