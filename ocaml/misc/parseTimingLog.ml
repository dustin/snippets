(*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: parseTimingLog.ml,v 1.1 2002/12/12 08:03:36 dustin Exp $
 *)

open Unix;;
open Stringutils;;

(* The type for log entries *)
type log_entry = {
	time: float;
	serial: string;
	ttype: string;
	state: string;
};;

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

(* Get a log entry from the line *)
let get_log_entry(l: string): log_entry =

	let parts = split(l, ' ', 12) in
		{
			(* line[0:23] *)
			time = parse_time(String.sub l 0 24);
			serial = List.nth parts 6;
			ttype = List.nth parts 7;
			state = List.nth parts 8;
		}
;;

(* Process a given log entry *)
let process(le: log_entry) =
	print_float(le.time);
	print_string(" - " ^ le.ttype ^ " from " ^ le.serial ^ " " ^ le.state);
	print_newline();
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

