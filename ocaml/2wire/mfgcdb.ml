(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 64a55096-d5ff-4ca3-a2e8-818c6f8027c8
 *)

(** Interface to manufacturing data *)

(** Exception thrown when a particular gateway cannot be found. *)
exception No_such_gateway of string;;

(** The magic meta inf key. *)
let magic_key = "meta_inf";;

(** The manufactured gateway type *)
type mfg = {
	gateway_key: int;
	sn: string;
	id_string: string;
	pca: string;
	model_num: int;
	product_string: string;
	version: string;
	auth_code: string;
	ssid: string;
	date_mod: string;
	wireless_id: string;
};;

(** Handle to a manufacturing db. *)
type mfg_db = {
	path: string;
	cdb: Cdb.cdb_file;
	mfg_version: int;
	cols: string list;
};;

(** Open a manufacturing cache. *)
let open_mfg_db path =
	let cdbi = Cdb.open_cdb_in path in
	let data = Cdb.find cdbi magic_key in
	let data_stream = Stream.of_string data in
	let mv = int_of_string (Netstring.decode data_stream) in
	let rec loop rv s =
		try
			let col = Netstring.decode s in
			loop (rv @ [col]) s
		with Stream.Failure ->
			rv
		in
	(* And the return structure *)
	{
		path=path;
		cdb=cdbi;
		mfg_version=mv;
		cols=loop [] (Stream.of_string (Netstring.decode data_stream))
	}
;;

(** Close the manufacturing db. *)
let close_mfg_db cdbi =
	Cdb.close_cdb_in cdbi.cdb
;;

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
		model_num=int_of_string (Hashtbl.find rv "modelnum");
		product_string=Hashtbl.find rv "productstring";
		version=Hashtbl.find rv "version";
		auth_code=Hashtbl.find rv "authcode";
		ssid=Hashtbl.find rv "ssid";
		date_mod=Hashtbl.find rv "moddate";
		wireless_id=Hashtbl.find rv "wirelessid";
	}
;;

(** Get a specific manufacturing record.

@param cdbi the cdb input record
@param sn the serial number of the gateway
*)
let lookup cdbi sn =
	try
		decode_record cdbi sn (Cdb.find cdbi.cdb sn)
	with Stream.Failure ->
		raise (No_such_gateway sn)
;;

(** Iterate all of the manufacturing records in the cdb.

@param f the function to call with each manufacturing record
@param cdbi the manufacturing cache
*)
let iter f cdbi =
	Cdb.iter (fun k v ->
			(*  Decode this value if it's not the magic key or a reverse
				mapping *)
			if k <> magic_key && (String.sub k 0 1) <> "r" then (
				f (decode_record cdbi k v)
			)
		) cdbi.path;
;;

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
