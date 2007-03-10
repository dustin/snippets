(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 64a55096-d5ff-4ca3-a2e8-818c6f8027c8
 *)

(** Interface to manufacturing data *)

(** Exception thrown when a particular gateway cannot be found. *)
exception No_such_gateway of string
(** Exception thrown when attempting to use this on something other than a
manufacturing cdb. *)
exception Invalid_cdb

(** The magic meta inf key. *)
let magic_key = "meta_inf"

(** The manufactured gateway type *)
type mfg = {
	gateway_key: int;
	sn: string;
	id_string: string;
	pca: string;
	model_num: string;
	product_string: string;
	version: string;
	auth_code: string;
	ssid: string;
	date_mod: string;
	wireless_id: string;
}

(** Handle to a manufacturing db. *)
type mfg_db = {
	paths: string array;
	cdbs: Cdb.cdb_file array;
	mfg_version: int;
	cols: string list;
}

let read_properties path =
	let rv = Hashtbl.create 1 in
	Fileutils.iter_file_lines (fun l ->
		let parts = Extstring.split l '=' 2 in
		Hashtbl.replace rv (List.hd parts) (List.nth parts 1)
		) (Printf.sprintf "%s/build.properties" path);
	rv

let count_cdbs path =
	int_of_string ((Hashtbl.find (read_properties path) "numfiles"))

let open_mfg_dbs version paths =
	let cdbs = Array.map Cdb.open_cdb_in paths in
	let data = try Cdb.find (Array.get cdbs 0) magic_key
		with Not_found -> raise Invalid_cdb in
	let data_stream = Stream.of_string data in
	let mv = int_of_string (Netstring.decode data_stream) in
	assert (mv = version);
	let rec loop rv s =
		try
			let col = Netstring.decode s in
			loop (rv @ [col]) s
		with Stream.Failure ->
			rv
		in
	{
		paths=paths;
		cdbs=cdbs;
		mfg_version=mv;
		cols=loop [] (Stream.of_string (Netstring.decode data_stream))
	}

(** Determine the version of the cdb at the given path *)
let determine_version path =
	try int_of_string ((Hashtbl.find (read_properties path) "version"))
	with Sys_error s -> 3

(** Open a manufacturing db. *)
let open_mfg_db path =
	if (determine_version path) > 3 then
		let num_cdbs = count_cdbs path in
		open_mfg_dbs 4 (Array.init num_cdbs
			(Printf.sprintf "%s/forward.%d.cdb" path))
	else
		open_mfg_dbs 3 (Array.make 1 path)

(** Close the manufacturing db. *)
let close_mfg_db cdbi =
	Array.iter Cdb.close_cdb_in cdbi.cdbs

(** Decode a record from a serial number match *)
let decode_record cdbi sn data =
	let data_s = Stream.of_string data in
	let rv = Hashtbl.create 1 in
	List.iter (fun n -> Hashtbl.add rv n (Netstring.decode data_s)) cdbi.cols;
	{
		gateway_key=int_of_string (Hashtbl.find rv "boxnum");
		sn=sn;
		id_string=Hashtbl.find rv "idstring";
		pca=Hashtbl.find rv "pca";
		model_num=(try Hashtbl.find rv "modelnum"
			with Not_found -> "0");
		product_string=Hashtbl.find rv "productstring";
		version=Hashtbl.find rv "version";
		auth_code=Hashtbl.find rv "authcode";
		ssid=Hashtbl.find rv "ssid";
		date_mod=Hashtbl.find rv "moddate";
		wireless_id=Hashtbl.find rv "wirelessid";
	}

let get_cdb_for_key_from_array cdbs k =
	let h = Int32.abs (Cdb.hash k) in
	let which = (Int32.to_int (Int32.rem h
		(Int32.of_int (Array.length cdbs)))) in
	Array.get cdbs which

(* Find the cdb to which this key applies *)
let get_cdb_for_key cdbi k =
	get_cdb_for_key_from_array cdbi.cdbs k

(** Get a specific manufacturing record.

@param cdbi the cdb input record
@param sn the serial number of the gateway
*)
let lookup cdbi sn =
	try
		decode_record cdbi sn (Cdb.find (get_cdb_for_key cdbi sn) sn)
	with Not_found ->
		raise (No_such_gateway sn)

(** Iterate all of the manufacturing records in the cdb.

@param f the function to call with each manufacturing record
@param cdbi the manufacturing db
*)
let iter f cdbi =
	Array.iter (fun path ->
		Cdb.iter (fun k v ->
				(*  Decode this value if it's not the magic key or a reverse
					mapping *)
				if k <> magic_key && (String.sub k 0 1) <> "r" then (
					f (decode_record cdbi k v)
				)
			) path) cdbi.paths

(*
let main() =
	let cdbFile = Sys.argv.(1) in

	print_endline("Loading " ^ cdbFile);
	let db = open_mfg_db cdbFile in

	Printf.printf "version: %d\n" db.mfg_version;
	List.iter print_endline db.cols;

	print_endline("Looking up a record:");
	let r = lookup db "153112000580" in
	Printf.printf "sn=%s version=%s auth=%s\n" r.sn r.version r.auth_code;

	print_endline("Iterating:");
	iter (fun m ->
		Printf.printf "sn=%s version=%s auth=%s\n" m.sn m.version m.auth_code)
		db;

	close_mfg_db db;
;;
*)
