(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 6BF17402-05DC-11D8-BEE8-000393DC8AE4
 *)

open Unix;;
open Hashtbl;;
open List;;
open Stringutils;;
open Fileutils;;

(* The type for log entries *)
type log_entry = {
	le_time: float;
	le_serial: string;
	le_ttype: string;
	le_state: string;
};;

(* A timing log entry (start to stop) *)
type log_timing = {
	lt_start: float;
	lt_stop: float;
	lt_ttype: string;
	lt_serial: string;
};;

(* The stuff we store in the massive per-block hash table *)
type per_block = {
	mutable pb_time: float;
	mutable pb_count: int;
	mutable pb_start: int;
	mutable pb_end: int;
};;

(* The types of logs we consider *)
let log_types = ["HB"; "BOOT"; "KICK"; "XMLRPC";];;

(* Log times with time/count/start/end appended *)
let extended_log_types =
	List.concat (List.map (function x ->
		(List.map (function y -> x ^ y) ["time";"count";"start";"end"]))
		log_types)
;;

(* This is the hashtable that will hold the state and stuff *)
let eventCache=Hashtbl.create 1;;
let perBlock=Hashtbl.create 1;;

(* Parse time from the given timestamp *)
let parse_time l =
	let times = split_chars l [' '; ':'; '-'; ','] 8 in
	fst(Unix.mktime {
		tm_sec = int_of_string(List.nth times 5);
		tm_min = int_of_string(List.nth times 4);
		tm_hour = int_of_string(List.nth times 3);
		tm_mday = int_of_string(List.nth times 2);
		tm_mon = (int_of_string(List.nth times 1) - 1);
		tm_year = (int_of_string(List.nth times 0) - 1900);
		tm_wday = 0;
		tm_yday = 0;
		tm_isdst = false
		}) +. (float_of_string(List.nth times 6) /. 1000.0)
;;

let approx_time t = 60 * ((int_of_float t) / 60) ;;

(* Get a log entry from the line *)
let get_log_entry l =
	let parts = split l ' ' 12 in
		{
			(* line[0:23] *)
			le_time = parse_time(String.sub l 0 24);
			le_serial = List.nth parts 6;
			le_ttype = List.nth parts 7;
			le_state = List.nth parts 8;
		}
;;

let get_log_timing le1 le2 =
	{
		lt_start = le1.le_time;
		lt_stop = le2.le_time;
		lt_ttype = le1.le_ttype;
		lt_serial = le1.le_serial;
	}
;;

(* Get the block that contains the counts for the given log entry.
 * This will also create any tables it needs.
 *)
let get_block le =
	try
		(* Look for the thing that matches this timestamp *)
		Hashtbl.find perBlock (approx_time le.le_time)
	with Not_found ->
		(* If we didn't have a match, build it all out *)
		let tmpBlock = Hashtbl.create 1 in
		List.iter (function x -> Hashtbl.add tmpBlock x {
			pb_time = 0.0;
			pb_count = 0;
			pb_start = 0;
			pb_end = 0;
			}) log_types;
		Hashtbl.add perBlock (approx_time le.le_time) tmpBlock;
		tmpBlock
;;

(* Get the item for recording the individual item *)
let get_item le =
	Hashtbl.find (get_block le) le.le_ttype
;;

(* Record the timing for this mofo *)
let record_timing le_old le_new =
	let lt = get_log_timing le_old le_new
	and li = get_item le_old in
	li.pb_count <- li.pb_count + 1;
	li.pb_time <- li.pb_time +. (lt.lt_stop -. lt.lt_start);
	();
;;

(* Process a given log entry *)
let process le =
	match le.le_state with
	| "start" ->
		begin
			let itmp = get_item le in
			itmp.pb_start <- itmp.pb_start + 1;
			try
				Hashtbl.find eventCache le.le_serial;
				prerr_endline("Duplicate start for " ^ le.le_serial);
				Hashtbl.remove eventCache le.le_serial;
			with Not_found -> ();
			Hashtbl.add eventCache le.le_serial le;
		end;
	| "end" ->
		begin
			let itmp = get_item le in
			itmp.pb_end <- itmp.pb_end + 1;
			try
				let old = Hashtbl.find eventCache le.le_serial in
				record_timing old le;
				Hashtbl.remove eventCache le.le_serial;
			with Not_found ->
				prerr_endline("No start for end " ^ le.le_serial);
		end;
	| _ -> raise (Invalid_argument le.le_state);
;;

let hashtbl_keys h = Hashtbl.fold (fun key _ l -> key :: l) h [];;

let print_block_header filename =
	print_string("update " ^ filename ^ " -t ");
	print_string(String.concat ":" extended_log_types);
	print_string(" ");
;;

let print_entry ts fn =
	let block = Hashtbl.find perBlock ts in
	print_block_header fn;
	print_int(ts);
	print_string(":");
	print_string(String.concat ":"
		(List.concat
			(List.map (function ltype ->
				let pb = Hashtbl.find block ltype in
				[string_of_float pb.pb_time;
				string_of_int pb.pb_count;
				string_of_int pb.pb_start;
				string_of_int pb.pb_end; ]) log_types)));
	print_newline()
;;

let print_output filename =
	List.iter (function e -> print_entry e filename)
		(List.sort compare (hashtbl_keys perBlock));
;;

(* do it *)
let main() =
	let rrd = (Array.get Sys.argv 1) in
	conditional_fold_lines
		(function l ->
			let le = get_log_entry(l) in
			try
				process(le);
			with Not_found -> prerr_endline("Type not found:  " ^ le.le_ttype);
			)
		(function l -> (strstr l "TransactionTiming" 40) >= 40)
		Pervasives.stdin;
	print_output rrd;
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;

