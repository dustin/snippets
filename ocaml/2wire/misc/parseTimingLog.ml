(*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: parseTimingLog.ml,v 1.1 2002/12/12 08:03:36 dustin Exp $
 *)

open Unix;;
open Hashtbl;;
open Stringutils;;

(* The type for log entries *)
type log_entry = {
	le_time: float;
	le_serial: string;
	le_ttype: string;
	le_state: string;
};;

type log_timing = {
	lt_start: float;
	lt_stop: float;
	lt_ttype: string;
	lt_serial: string;
};;

(* This is the hashtable that will hold the state and stuff *)
let eventCache=Hashtbl.create 1;;
(*
let perBlock=Hashtbl.create 1;;
*)

(* Parse time from the given timestamp *)
let parse_time(l: string): float =
	let times = split_chars(l, [' '; ':'; '-'; ','], 8) in
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

let approx_time(lt: log_timing): int =
	60 * ((int_of_float lt.lt_start) / 60)
;;

(* Get a log entry from the line *)
let get_log_entry(l: string): log_entry =

	let parts = split(l, ' ', 12) in
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

(* Record the timing for this mofo *)
let record_timing le_old le_new =
	prerr_string("Recording " ^ le_old.le_ttype ^ " of "
		^ le_old.le_serial ^ " from ");
	prerr_float(le_old.le_time);
	prerr_string(" to ");
	prerr_float(le_new.le_time);
	prerr_string(" ");
	prerr_float(le_new.le_time -. le_old.le_time);
	prerr_endline("s");

(*
	let lt = get_log_timing le_old le_new in
	try
		let pb = Hashtbl.find perBlock lt.lt_serial in
	with Not_found ->
		Hashtbl.add perBlock approx_time(lt) {
				pb_ts = approx_time(lt);
				pb_counts = 1;
				pb_times = (lt.lt_stop - lt.lt_start);
			}
*)
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

(* do it *)
let main() =
	let rrd = (Array.get Sys.argv 1) in
	try
		while true do
			let l = (read_line()) in
			if strstr(l, "TransactionTiming", 40) >= 40 then
				process(get_log_entry(l))
		done;
	with End_of_file -> ignore();
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;

