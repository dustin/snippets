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

type model_record = {
	mr_id: string;
	mr_product_line: string;
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

(* Reserved keys *)
let reservedKeys = ref None;;

(* When we collide, we hand out keys from the top.  This is the highest one
   potentially available to be handed out. *)
let collisionKey = ref max_int;;

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
		(fun x -> (Netstring.encode (Hashtbl.find ht x))) cdb_fields)),
		ht
;;

(* Check to see if this is a reserved number.  This will only be called for
   a generated number, so if it's in the reserve DB, it's wrong *)
let checkReservedDup n plsn =
	match !reservedKeys with
		  None -> false
		| Some(cdb) ->
			try
				let found = Cdb.find cdb (string_of_int n) in
				Printf.eprintf "DUPLICATE (reserved) AT %d (old: %s, new: %s)\n"
					n found plsn;
				true
			with Stream.Failure ->
				false
;;

(* Check for a duplicate generated key. *)
let checkUsable n plsn =
	try
		let found = Hashtbl.find seenIds n in
		if (plsn = found) then (
			raise (Duplicate plsn)
		) else (
			Printf.eprintf "DUPLICATE AT %d (old: %s, new: %s)\n" n found plsn;
			false
		)
	with Not_found ->
		if checkReservedDup n plsn then (
			false
		) else (
			Hashtbl.add seenIds n plsn;
			true
		)
;;

(* Return an unused key potentially related to the provided suggestion *)
let resolveKey start plsn =
	if (checkUsable start plsn) then (
		start
	) else (
		(* Find the largest unused key *)
		let rec loop n =
			if checkUsable n plsn then (
				collisionKey := (n - 1);
				n
			) else (
				loop (n - 1)
			)
		in loop !collisionKey
	)
;;

(* Generate a box number from a productline/sn pair *)
let genBoxNum product_line sn =
	let plsn = (product_line ^ "," ^ sn) in
	let dig = String.get (Digest.string plsn) in
	let a,b,c,d =	(Char.code (dig 0)), (Char.code (dig 1)),
					(Char.code (dig 2)), (Char.code (dig 3)) in
	resolveKey (abs(a lor (b lsl 8) lor (c lsl 16) lor (d lsl 24))) plsn
;;

(* Get the box number for the given productline/serial number.  This will
   either be pulled from the reserve DB (if specified), or will be generated.
 *)
let getBoxNum product_line sn =
	match !reservedKeys with
		  None ->
			(* If we don't have a reserved db, don't use it *)
			genBoxNum product_line sn
		| Some(cdb) ->
			try
				(* Try to get it from the reserved key cdb *)
				int_of_string (Cdb.find cdb (product_line ^ "," ^ sn))
			with Stream.Failure ->
				(* If it's not there, generate one *)
				genBoxNum product_line sn
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
	(* Parse the record.  Split and remove all the whitespace *)
	let parts = nthDefault (List.map Extstring.strip
								(Extstring.split_all l '|' 99)) "" in
	let record = {	in_sn=parts 0;
					in_version=parts 1;
					in_secret=parts 2;
					in_model=parts 3;
					in_password=parts 4;
					in_wirelessid=parts 5} in
	try
		let mr_rec = (try
				Hashtbl.find modelMap record.in_model
			with x ->
				if String.length record.in_model = 15 then
					Hashtbl.find modelMap (String.sub record.in_model 0 11)
				else
					raise x
			) in
		(* Convenience function for updating the hashtable *)
		let htr = Hashtbl.replace ht in
		htr "boxnum" (string_of_int
						(getBoxNum mr_rec.mr_product_line record.in_sn));
		htr "modelnum" mr_rec.mr_id;
		htr "pca" mr_rec.mr_id;
		htr "version" record.in_version;
		htr "authcode" record.in_secret;
		htr "moddate" ts;
		Some (makeOutRecord mr_rec.mr_product_line record.in_sn ht)
	with Not_found ->
		prerr_endline("Unknown model:  " ^ record.in_model);
		None
;;

(* Store a parsed record *)
let storeRecord db modelMap ts l =
	match (makeRecord modelMap ts l) with
	  None -> ()
	| Some(k,v,ht) ->
		Printf.printf "%s|%s\n" (Hashtbl.find ht "boxnum") k;
		Cdb.add db k v
;;

(* Parse a model map line *)
let parseModelMap ht l =
	try
		let parts = List.nth (Extstring.split_all l '|' 4) in
		Hashtbl.add ht (parts 0) {mr_id=parts 1; mr_product_line=parts 2};
	with Failure("nth") ->
		Printf.eprintf "Error on model map line:  ``%s''\n" l
;;

(* Process a specific file *)
let processFile destcdb modelMap filename =
	let tm = Unix.gmtime (Unix.stat filename).st_mtime in
	let ts = Printf.sprintf "%04d%02d%02dT%02d%02d%02d"
			(tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
			tm.tm_hour tm.tm_min tm.tm_sec in
	Fileutils.iter_file_lines (storeRecord destcdb modelMap ts) filename

;;

(* Return the listing of a directory in a predictable order so we process
   files in the same order *)
let sorted_ls d =
	List.sort compare (Fileutils.lsdir d)
;;

(* Process the files and directories passed in as anonymous arguments *)
let processPaths destcdb modelMap paths =
	List.iter (fun path ->
			if Fileutils.isdir path then (
				Fileutils.walk_dir_via sorted_ls path (fun d files arg ->
						List.iter (processFile destcdb modelMap)
							(List.map (Filename.concat d) files)) ()
			) else (
				processFile destcdb modelMap path
			)
		) paths
;;

let usage() =
	prerr_endline("Usage:  " ^ Sys.argv.(0)
		^ " -d destcdb -m modelmap [-k mfgkeys] inputpath [inputpath ...]");
	exit(1)
;;

(* If the user wants to use a reserved path DB, this will set it up *)
let setupReserved path =
	reservedKeys := Some(Cdb.open_cdb_in path)
;;

(* This is where all the work's done, main *)
let main () =
	let destpath = ref "" in
	let modelpath = ref "" in
	let anon = ref [] in
	Arg.parse [
			("-d", Arg.Set_string(destpath), "Location of the destination cdb");
			("-m", Arg.Set_string(modelpath), "Location of the model map");
			("-k", Arg.String(setupReserved),
					"Location of the reserved mfg keys cdb")
		] (fun s -> anon := s :: !anon)  "Build manufacturing cache";
	if("" = !destpath) then
		usage();
	if("" = !modelpath) then
		usage();
	if([] = !anon) then
		usage();

	(* Open the destination cdb *)
	let destcdb = Cdb.open_out !destpath in
	(* add the meta_inf *)
	Cdb.add destcdb magic_key (makeMetaInf());
	(* Initialize the model map *)
	let modelMap = Hashtbl.create 1 in
	Fileutils.iter_file_lines (parseModelMap modelMap) !modelpath;
	(* process records *)
	processPaths destcdb modelMap (List.rev !anon);
	(* Close and cleanup *)
	Cdb.close_cdb_out destcdb
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

