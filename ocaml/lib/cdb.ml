(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 1E3B7401-2AE1-11D8-A379-000393CB0F1E
 *)

(** CDB Implementation.
 http://cr.yp.to/cdb/cdb.txt
 *)

(* The cdb hash function is ``h = ((h << 5) + h) ^ c'', with a starting
   hash of 5381.
 *)

type cdb_creator = {
	table_count: int array;
	mutable pointers: (int * int) list;
	out: out_channel;
};;

(** Initial hash value *)
let hash_init = 5381;;

(** Hash the given string. *)
let hash s =
	let h = ref hash_init in
	String.iter (fun c -> h := ((!h lsl 5) + !h) lxor (int_of_char c)) s;
	!h
;;

(** Write a little endian integer to the file *)
let write_le cdc i =
	output_byte cdc.out (i land 0xff);
	output_byte cdc.out ((i lsr 8) land 0xff);
	output_byte cdc.out ((i lsr 16) land 0xff);
	output_byte cdc.out ((i lsr 24) land 0xff)
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

(** Add a value to the cdb *)
let add cdc k v =
	(* Add the data to the file *)
	write_le cdc (String.length k);
	write_le cdc (String.length v);
	output_string cdc.out k;
	output_string cdc.out v;

	(* Add the hash to the list *)
	let h = hash k in
	cdc.pointers <- cdc.pointers @ [(h, pos_out cdc.out)];
	cdc.table_count.(h land 0xff) <- cdc.table_count.(h land 0xff) + 1;
;;

(** Is this option none? *)
let is_none = function
	None -> true
	| _ -> false
;;

(** Is this option some? *)
let is_some = function
	Some(x) -> true
	| _ -> false
;;

exception Empty_option;;

(** Get an option value  *)
let get_option o = function
	Some(x) -> x
	| None -> raise Empty_option
;;

(** Process a hash table *)
let process_table cdc table_start slot_table slot_pointers i tc =
	(* Length of the table *)
	let len = tc * 2 in
	(* Store the table position *)
	slot_table := !slot_table @ [(pos_out cdc.out, len)];
	(* Build the hash table *)
	let ht = Array.make len None in
	let cur_p = ref table_start.(i) in
	(* from 0 to tc-1 because the loop will run an extra time otherwise *)
	for u = 0 to (tc - 1) do
		let hp = slot_pointers.(!cur_p) in
		cur_p := !cur_p + 1;

		(* Find an available hash bucket *)
		let rec find_where where =
			if (is_none ht.(where)) then
				where
			else (
				if ((where + 1) = len) then (find_where 0)
				else (find_where (where + 1))
			) in
		let where = find_where (((fst hp) / 256) mod len) in
		ht.(where) <- Some hp;
	done;
	(* Write this hash table *)
	Array.iter (fun hpp ->
		match hpp with
		  None ->
			write_le cdc 0;
			write_le cdc 0;
		| Some(hp) ->
			write_le cdc (fst hp);
			write_le cdc (snd hp)
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
	(* Build out the slot pointers array *)
	let slot_pointers = Array.make (List.length cdc.pointers) (0,0) in
	(* Fill in the slot pointers *)
	List.iter (fun hp ->
		let h = fst hp in
		table_start.(h land 0xff) <- table_start.(h land 0xff) - 1;
		slot_pointers.(table_start.(h land 0xff)) <- hp
		) cdc.pointers;
	(* Write the shit out *)
	let slot_table = ref [] in
	(* Write out the hash tables *)
	Array.iteri (process_table cdc table_start slot_table slot_pointers)
		cdc.table_count;
	(* write out the pointer sets *)
	seek_out cdc.out 0;
	List.iter (fun x -> write_le cdc (fst x); write_le cdc (snd x))
		!slot_table;
	close_out cdc.out
;;

(** test app to create ``test.cdb'' and put some stuff in it *)
let main() =
	let c = open_out "test.cdb" in
	add c "a" "1";
	add c "b" "2";
	add c "c" "3";
	close_cdb_out c;
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;
