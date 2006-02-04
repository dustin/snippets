(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 1E3B7401-2AE1-11D8-A379-000393CB0F1E
 *)

(** CDB Implementation.
 {{:http://cr.yp.to/cdb/cdb.txt} http://cr.yp.to/cdb/cdb.txt}
 *)

(* The cdb hash function is ``h = ((h << 5) + h) ^ c'', with a starting
   hash of 5381.
 *)

(** CDB creation handle. *)
type cdb_creator = {
	table_count: int array;
	(* Hash index pointers *)
	mutable pointers: (Int32.t * Int32.t) list;
	out: out_channel;
};;

(** Initial hash value *)
let hash_init = Int64.of_int 5381;;

let ff64 = Int64.of_int 0xff;;
(* I need to do this of_string because it's larger than an ocaml int *)
let ffffffff64 = Int64.of_string "0xffffffff";;
let ff32 = Int32.of_int 0xff;;

(** Hash the given string. *)
let hash s =
	let h = ref hash_init in
	String.iter (fun c -> h := Int64.logand ffffffff64 (Int64.logxor
								(Int64.add (Int64.shift_left !h 5) !h)
								(Int64.of_int (int_of_char c)))
						) s;
	Int64.to_int32 !h
;;

let write_le cdc i =
	output_byte cdc.out (i land 0xff);
	output_byte cdc.out ((i lsr 8) land 0xff);
	output_byte cdc.out ((i lsr 16) land 0xff);
	output_byte cdc.out ((i lsr 24) land 0xff)
;;

(** Write a little endian integer to the file *)
let write_le32 cdc i =
	output_byte cdc.out (Int32.to_int (Int32.logand ff32 i));
	output_byte cdc.out (Int32.to_int
	 (Int32.logand ff32 (Int32.shift_right_logical i 8)));
	output_byte cdc.out (Int32.to_int
	 (Int32.logand ff32 (Int32.shift_right_logical i 16)));
	output_byte cdc.out (Int32.to_int
		(Int32.logand ff32 (Int32.shift_right_logical i 24)))
;;

(**
	Open a cdb creator for writing.

	@param fn the file to write
 *)
let open_out fn =
	let s = {	table_count=Array.make 256 0;
				pointers=[];
				out=open_out_bin fn
			} in
	(* Skip over the header *)
	seek_out s.out 2048;
	s
;;

(**
	Convert out_channel to cdb_creator.

	@param out_channel the out_channel to convert
 *)
let cdb_creator_of_out_channel out_channel =
	let s = {	table_count=Array.make 256 0;
				pointers=[];
				out=out_channel
			} in
	(* Skip over the header *)
	seek_out s.out 2048;
	s
;;


let hash_to_table h =
	Int32.to_int (Int32.logand h ff32)
;;

let hash_to_bucket h len =
	Int32.to_int (Int32.rem (Int32.shift_right_logical h 8) (Int32.of_int len))
;;

let pos_out_32 x =
	Int64.to_int32 (LargeFile.pos_out x)
;;

(** Add a value to the cdb *)
let add cdc k v =
	(* Add the hash to the list *)
	let h = hash k in
	cdc.pointers <- (h, pos_out_32 cdc.out) :: cdc.pointers;
	let table = hash_to_table h in
	cdc.table_count.(table) <- cdc.table_count.(table) + 1;

	(* Add the data to the file *)
	write_le cdc (String.length k);
	write_le cdc (String.length v);
	output_string cdc.out k;
	output_string cdc.out v
;;

(** Process a hash table *)
let process_table cdc table_start slot_table slot_pointers i tc =
	(* Length of the table *)
	let len = tc * 2 in
	(* Store the table position *)
	slot_table := (pos_out_32 cdc.out, Int32.of_int len) :: !slot_table;
	(* Build the hash table *)
	let ht = Array.make len None in
	let cur_p = ref table_start.(i) in
	(* Lookup entries by slot number *)
	let lookupSlot x =
		try Hashtbl.find slot_pointers x
		with Not_found -> (Int32.zero,Int32.zero)
	in
	(* from 0 to tc-1 because the loop will run an extra time otherwise *)
	for u = 0 to (tc - 1) do
		let hp = lookupSlot !cur_p in
		cur_p := !cur_p + 1;

		(* Find an available hash bucket *)
		let rec find_where where =
			if (Extoption.is_none ht.(where)) then (
				where
			) else (
				if ((where + 1) = len) then (find_where 0)
				else (find_where (where + 1))
			) in
		let where = find_where (hash_to_bucket (fst hp) len) in
		ht.(where) <- Some hp;
	done;
	(* Write this hash table *)
	Array.iter (fun hpp ->
			let h,t = match hpp with
				None -> Int32.zero,Int32.zero
				| Some(h,t) -> h,t;
			in
			write_le32 cdc h; write_le32 cdc t
		) ht;
;;

(** Close and finish the cdb creator. *)
let close_cdb_out cdc =
	let cur_entry = ref 0 in
	let table_start = Array.make 256 0 in
	(* Find all the hash starts *)
	Array.iteri (fun i x ->
		cur_entry := !cur_entry + x;
		table_start.(i) <- !cur_entry) cdc.table_count;
	(* Build out the slot pointers hash *)
	let slot_pointers = Hashtbl.create (List.length cdc.pointers) in
	(* Fill in the slot pointers *)
	List.iter (fun hp ->
		let h = fst hp in
		let table = hash_to_table h in
		table_start.(table) <- table_start.(table) - 1;
		Hashtbl.replace slot_pointers table_start.(table) hp;
		) cdc.pointers;
	(* Write the shit out *)
	let slot_table = ref [] in
	(* Write out the hash tables *)
	Array.iteri (process_table cdc table_start slot_table slot_pointers)
		cdc.table_count;
	(* write out the pointer sets *)
	seek_out cdc.out 0;
	List.iter (fun x -> write_le32 cdc (fst x); write_le32 cdc (snd x))
		(List.rev !slot_table);
	close_out cdc.out
;;

(** {1 Iterating a cdb file} *)

(* read a little-endian integer *)
let read_le f =
	let a = (input_byte f) in
	let b = (input_byte f) in
	let c = (input_byte f) in
	let d = (input_byte f) in
	a lor (b lsl 8) lor (c lsl 16) lor (d lsl 24)
;;

(* Int32 version of read_le *)
let read_le32 f =
	let a = (input_byte f) in
	let b = (input_byte f) in
	let c = (input_byte f) in
	let d = (input_byte f) in
	Int32.logor (Int32.of_int (a lor (b lsl 8) lor (c lsl 16)))
				(Int32.shift_left (Int32.of_int d) 24)
;;

(**
 Iterate a CDB.

 @param f the function to call for every key/value pair
 @param fn the name of the cdb to iterate
 *)
let iter f fn =
	let fin = open_in_bin fn in
	try
		(* Figure out where the end of all data is *)
		let eod = read_le32 fin in
		(* Seek to the record section *)
		seek_in fin 2048;
		let rec loop() =
			(* (pos_in fin) < eod *)
			if (Int32.compare (Int64.to_int32 (LargeFile.pos_in fin)) eod < 0)
				then (
				let klen = read_le fin in
				let dlen = read_le fin in
				let key = String.create klen in
				let data = String.create dlen in
				really_input fin key 0 klen;
				really_input fin data 0 dlen;
				f key data;
				loop()
			) in
		loop();
		close_in fin;
	with x -> close_in fin; raise x;
;;

(** {1 Searching } *)

(** Type type of a cdb_file. *)
type cdb_file = {
	f: in_channel;
	(* Position * length *)
	tables: (Int32.t * int) array;
};;

(** Open a CDB file for searching.

 @param fn the file to open
 *)
let open_cdb_in fn =
	let fin = open_in_bin fn in
	let tables = Array.make 256 (Int32.zero,0) in
	(* Set the positions and lengths *)
	Array.iteri (fun i it ->
		let pos = read_le32 fin in
		let len = read_le fin in
		tables.(i) <- (pos,len)
		) tables;
	{f=fin; tables=tables}
;;

(**
 Close a cdb file.

 @param cdf the cdb file to close
 *)
let close_cdb_in cdf =
	close_in cdf.f
;;

(** Get a stream of matches.

 @param cdf the cdb file
 @param key the key to search
 *)
let get_matches cdf key =
	let kh = hash key in
	(* Find out where the hash table is *)
	let hpos, hlen = cdf.tables.(hash_to_table kh) in
	let incr_slot x = (if (1 + x) > hlen then 0 else (1 + x)) in
	let rec loop x =
		if(x >= hlen) then (
			None
		) else (
			(* Calculate the slot containing these entries *)
			let lslot = ((hash_to_bucket kh hlen) + x) mod hlen in
			let spos = Int32.add (Int32.of_int (lslot * 8)) hpos in
			LargeFile.seek_in cdf.f (Int64.of_int32 spos);
			let h = read_le32 cdf.f in
			let pos = read_le32 cdf.f in
			(* validate that we a real bucket *)
			if (h = kh) && ((Int32.compare pos Int32.zero) > 0) then (
				LargeFile.seek_in cdf.f (Int64.of_int32 pos);
				let klen = read_le cdf.f in
				if (klen = String.length key) then (
					let dlen = read_le cdf.f in
					let rkey = String.create klen in
					really_input cdf.f rkey 0 klen;
					if(rkey = key) then (
						let rdata = String.create dlen in
						really_input cdf.f rdata 0 dlen;
						Some(rdata)
					) else (
						loop (x + 1)
					)
				) else (
					loop (x + 1)
				)
			) else (
				loop (x + 1)
			)
		) in
	Stream.from loop
;;

(**
 Find the first record with the given key.

 @param cdf the cdb_file
 @param key the key to find
 *)
let find cdf key =
	try
		Stream.next (get_matches cdf key)
	with Stream.Failure ->
		raise Not_found
;;
