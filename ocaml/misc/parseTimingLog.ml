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
	mutable pb_count: int;
	mutable pb_time: float;
};;

(* The types of logs we consider *)
let logTypes = ["HB"; "BOOT"; "KICK"; "XMLRPC";];;

(* Log times with time/count/start/end appended *)
let extended_log_types =
	List.concat (List.map (function x ->
		(List.map (function y -> x ^ y) ["time";"count";"start";"end"]))
		logTypes)
;;

(* This is the hashtable that will hold the state and stuff *)
let eventCache=Hashtbl.create 1;;
let perBlock=Hashtbl.create 1;;

(* Parse time from the given timestamp *)
let parse_time(l: string): float =
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
let get_log_entry(l: string): log_entry =

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

(* Block maker *)
let make_block() = { pb_count = 0; pb_time = 0.0; }

(* Get the block that contains the counts for the given log entry.
 * This will also create any tables it needs.
 *)
let get_block le =
	try
		(* Look for the thing that matches this timestamp *)
		let dateBlock = Hashtbl.find perBlock (approx_time le.le_time) in
		Hashtbl.find dateBlock le.le_ttype;
	with Not_found ->
		(* If we didn't have a match, build it all out *)
		let tmpHash = Hashtbl.create 1 in
		List.iter (function x -> Hashtbl.add tmpHash x (make_block())) logTypes;
		Hashtbl.add perBlock (approx_time le.le_time) tmpHash;
		Hashtbl.find tmpHash le.le_ttype;
;;

(* Record the timing for this mofo *)
let record_timing le_old le_new =
	(*
	prerr_string("Recording " ^ le_old.le_ttype ^ " of "
		^ le_old.le_serial ^ " from ");
	prerr_float(le_old.le_time);
	prerr_string(" to ");
	prerr_float(le_new.le_time);
	prerr_string(" ");
	prerr_float(le_new.le_time -. le_old.le_time);
	prerr_endline("s");
	*)

	let lt = get_log_timing le_old le_new in
	let pb = get_block le_old in
	pb.pb_count <- pb.pb_count + 1;
	pb.pb_time <- pb.pb_time +. (lt.lt_stop -. lt.lt_stop);
	();
;;

(* Process a given log entry *)
let process(le: log_entry) =
	match le.le_state with
	| "start" ->
		begin
			try
				Hashtbl.find eventCache le.le_serial;
				prerr_endline("Duplicate start for " ^ le.le_serial);
				Hashtbl.remove eventCache le.le_serial;
			with Not_found -> ();
			Hashtbl.add eventCache le.le_serial le;
		end;
	| "end" ->
		begin
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

let print_output filename =
	print_block_header filename;
	(* XXX:  Print map here *)
;;

(* do it *)
let main() =
	let rrd = (Array.get Sys.argv 1) in
	conditional_fold_lines
		(function l -> process(get_log_entry(l)))
		(function l -> (strstr l "TransactionTiming" 40) >= 40)
		Pervasives.stdin;
	print_output rrd;
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;

