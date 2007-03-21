(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: F31E0A41-A042-11D8-BB34-000393CFE6B8
 *)

open Unix

exception Duplicate of string

type in_record = {
	in_sn: string;
	in_version: string;
	in_secret: string;
	in_model: string;
	in_password: string;
	in_wirelessid: string;
}

type model_record = {
	mr_id: string;
	mr_product_line: string;
}

(* The output fields *)
let cdb_fields = [
	"boxnum"; "modelnum"; "idstring"; "pca"; "productstring";
	"version"; "authcode"; "ssid"; "wirelessid"; "moddate"
]

let magic_key = "meta_inf"
let version = "4"

(* The column from the pca.dat file that will be included as the PCA *)
let pcaPart = ref 1

(* The mapping of seen IDs *)
let seenIds = Hashtbl.create 1

(* Reserved keys *)
let reservedKeys = Hashtbl.create 1

(* Store bad models we've already processed here *)
let badModels = Hashtbl.create 1

(* When we collide, we hand out keys from the top.  This is the highest one
   potentially available to be handed out. *)
let nextKey = ref 1

(* The meta_inf is the version number followed by a netstring containing the
	list of fields as netstrings *)
let makeMetaInf () =
	(Netstring.encode version) ^ (Netstring.encode
		(List.fold_left (fun l a -> (Netstring.encode a) ^ l) ""
			(* Reverse the fields since we're folding and will be building
				backwards *)
			(List.rev cdb_fields)))

let makeOutRecord product_line sn ht =
	(product_line ^ "," ^ sn), (String.concat "" (List.map
		(fun x -> (Netstring.encode (Hashtbl.find ht x))) cdb_fields)),
		ht

(* Get the box number for the given productline/serial number.  This will
   either be pulled from the reserve DB (if specified), or will be generated.
 *)
let getBoxNum product_line sn =
	try
		(* Try to get it from the reserved key cdb *)
		Hashtbl.find reservedKeys (product_line ^ "," ^ sn)
	with Not_found ->
		(* If it's not there, generate one *)
		let rv = !nextKey in
		nextKey := rv + 1;
		rv

(* Like List.nth, but with a default *)
let nthDefault l def n =
	try
		List.nth l n
	with Failure("nth") ->
		def

(* Report a bad model if we haven't reported it already *)
let reportBadModel m =
	try
		Hashtbl.replace badModels m (1 + (Hashtbl.find badModels m))
	with Not_found ->
		Hashtbl.replace badModels m 1

(* Print all of the bad models with their counts *)
let printMissingModels () =
	Hashtbl.iter (fun k v ->
		Printf.eprintf "Unknown model ``%s'':  %d\n" k v
	) badModels

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
		htr "version" ""; (* Don't include the version in the cdb anymore *)
		htr "authcode" record.in_password;
		htr "idstring" record.in_secret;
		htr "moddate" ts;
		Some (makeOutRecord mr_rec.mr_product_line record.in_sn ht)
	with Not_found ->
		reportBadModel record.in_model;
		None

(* Find the cdb to which this key applies *)
let get_cdb_for_key cdbs k =
	let h = Int32.abs (Cdb.hash k) in
	let which = (Int32.to_int (Int32.rem h
		(Int32.of_int (Array.length cdbs)))) in
	Array.get cdbs which

(* Store a parsed record *)
let storeRecord rcdb cdbs modelMap ts l =
	try
		match (makeRecord modelMap ts l) with
		  None -> ()
		| Some(k,v,ht) ->
			let boxnum = (Hashtbl.find ht "boxnum") in
			Printf.printf "%s|%s\n" boxnum k;
			(* Add the reverse lookup hint *)
			Cdb.add rcdb ("r" ^ boxnum) k;
			(* Add the forward lookup *)
			Cdb.add (get_cdb_for_key cdbs k) k v
	with x ->
		Printf.eprintf "Error processing line %s\n" l;
		raise x

(* Parse a model map line *)
let parseModelMap ht l =
	try
		let parts = List.nth (Extstring.split_all l '|' 4) in
		Hashtbl.add ht (parts 0) {	mr_id=parts !pcaPart;
									mr_product_line=parts 2};
	with Failure("nth") ->
		Printf.eprintf "Error on model map line:  ``%s''\n" l

(* Get the timestamp of a file.  This will try to use it based on the name of
   the file, otherwise it will fall back to stat *)
let getTimestamp filename =
	try
		(* Get the last ten characters *)
		let last10 = String.sub filename ((String.length filename) - 10) 10 in
		(* Extract and format *)
		Scanf.sscanf last10 "%04d%02d%02d%02d"
			(Printf.sprintf "%04d%02d%02dT%02d0000")
	with _ ->
		(* If there's any failure at all doing the above parsing, just fall
			back to stat *)
		let tm = Unix.gmtime (Unix.stat filename).st_mtime in
		(* Format it *)
		Printf.sprintf "%04d%02d%02dT%02d%02d%02d"
				(tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
				tm.tm_hour tm.tm_min tm.tm_sec

(* Process a specific file *)
let processFile rcdb cdbs modelMap filename =
	if not (Extstring.begins_with filename ".") then
		let ts = getTimestamp filename in
		Fileutils.iter_file_lines (storeRecord rcdb cdbs modelMap ts) filename

(* Return the listing of a directory in a predictable order so we process
   files in the same order *)
let sorted_ls d =
	List.sort compare (Fileutils.lsdir d)

(* Process the files and directories passed in as anonymous arguments *)
let processPaths rcdb cdbs modelMap paths =
	List.iter (fun path ->
			if Fileutils.isdir path then (
				Fileutils.walk_dir_via sorted_ls path (fun d files arg ->
						List.iter (processFile rcdb cdbs modelMap)
							(List.map (Filename.concat d) files)) ()
			) else (
				processFile rcdb cdbs modelMap path
			)
		) paths

let usage() =
	prerr_endline("Usage:  " ^ Sys.argv.(0)
		^ " -d destcdb -m modelmap [-k mfgkeys] [-n num_out_files] inputpath [inputpath ...]");
	exit(1)

(* If the user wants to use a reserved path DB, this will set it up *)
let setupReserved path =
	Fileutils.iter_file_lines (fun l ->
			try
				let parts = List.nth (Extstring.split_all l '|' 3) in
				let id = int_of_string(parts 0) and sn = parts 1 in
				Hashtbl.replace reservedKeys sn id;
				Hashtbl.replace seenIds id sn;
				if (id >= !nextKey) then (
					nextKey := id + 1
				)
			with x ->
				Printf.eprintf "Error on ``%s'':  %s\n" l (Printexc.to_string x)
		) path;
	Printf.eprintf "Next key is %d\n" !nextKey

let build_props path num =
	let tm = Unix.gmtime (Unix.time ()) in
	let f = open_out (path ^ "/build.properties") in
	Printf.fprintf f "version=%s\n" version;
	Printf.fprintf f "numfiles=%d\n" num;
	Printf.fprintf f "builddate=%04d%02d%02dT%02d%02d%02d\n"
				(tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
				tm.tm_hour tm.tm_min tm.tm_sec;
	close_out f

let make_forward_cdb path n =
	let cdb = Cdb.open_out (Printf.sprintf "%s/forward.%d.cdb" path n) in
	Cdb.add cdb magic_key (makeMetaInf());
	cdb

(* This is where all the work's done, main *)
let main () =
	let destpath = ref "" in
	let modelpath = ref "" in
	let numfiles = ref 3 in
	let anon = ref [] in
	Arg.parse [
			("-d", Arg.Set_string(destpath), "Location of the destination cdb");
			("-m", Arg.Set_string(modelpath), "Location of the model map");
			("-k", Arg.String(setupReserved),
					"Location of the reserved mfg keys -> sn map");
			("-n", Arg.Set_int(numfiles),
					"How many forward files to produce.");
			("-p", Arg.Unit(fun _ -> pcaPart := 0),
					"Include a literal PCA instead of the ID.")
		] (fun s -> anon := s :: !anon)  "Build manufacturing DB";
	if("" = !destpath) then
		usage();
	if("" = !modelpath) then
		usage();
	if([] = !anon) then
		usage();

	Unix.mkdir !destpath 0o755;

	(* Record the build properties *)
	build_props !destpath !numfiles;

	(* Get the reverse cdb *)
	let rcdb = Cdb.open_out (Printf.sprintf "%s/reverse.cdb" !destpath) in
	(* Create the forward indexes *)
	let cdbs = Array.init !numfiles (make_forward_cdb !destpath) in

	(* Initialize the model map *)
	let modelMap = Hashtbl.create 1 in
	Fileutils.iter_file_lines (parseModelMap modelMap) !modelpath;

	(* process records *)
	processPaths rcdb cdbs modelMap (List.rev !anon);

	(* Go ahead and dump the hashtable to make a bit of room for indexing *)
	Hashtbl.clear seenIds;
	Hashtbl.clear reservedKeys;

	Printf.eprintf "Indexing...\n";
	(* Close the reverse indexes *)
	Cdb.close_cdb_out rcdb;
	(* Close the forward indexes *)
	Array.iter Cdb.close_cdb_out cdbs;
	(* Display all of the stuff we didn't found that was interesting *)
	printMissingModels()
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
