(* Copyright (c) 2002  Dustin Sallings <dustin@spy.net> *)
(* $Id: parseSQLLog.ml,v 1.2 2002/12/10 09:53:39 dustin Exp $ *)
(* 2wire SQL Log parser *)

open Unix;;

(** Log entry representing a single line from the file *)
type log_entry = {
	time: int;
	calls: int;
	calltime: int;
};;

type stats = {
	mutable last_time: int;
	mutable total_calls: int;
	mutable total_time: int;
};;

(* Global regexes that will be used *)
let g_timesep = Str.regexp "[-: ]";;
let g_spacesep = Str.regexp "[ ]+";;
let g_slashsep = Str.regexp "[/]";;

(* True if the line looks like a SQL log entry *)
let is_log_entry(l: string): bool =
	(* Look for the exact location since there's no strstr in ocaml *)
	String.length l > 60
		&& ((String.sub l 36 22) = "database.DBManager.sql")
;;

(* Parse time from the given timestamp *)
let parse_time(l: string): int =
	let times = Str.split g_timesep l in
	int_of_float(fst(Unix.mktime {
		tm_sec = int_of_string(List.nth times 5);
		tm_min = int_of_string(List.nth times 4);
		tm_hour = int_of_string(List.nth times 3);
		tm_mday = int_of_string(List.nth times 2);
		tm_mon = (int_of_string(List.nth times 1) - 1);
		tm_year = (int_of_string(List.nth times 0) - 1900);
		tm_wday = 0;
		tm_yday = 0;
		tm_isdst = false
		}))
;;

(* Get a log entry from the line *)
let get_log_entry(l: string): log_entry =

	let parts = Str.split g_spacesep l in
	let tparts = Str.split g_slashsep (List.nth parts 10) in
		{
			(* line[0:19] *)
			time = parse_time(String.sub l 0 19);
			(* line_array[10][1] *)
			calls = int_of_string(List.nth tparts 1);
			(* line_array[10][0] *)
			calltime =
				int_of_string(String.sub (List.hd tparts) 0
					((String.length (List.hd tparts)) - 2))
		}
;;

(* Round to the nearest minute *)
let get_approx_time(le: log_entry): int =
	(le.time / 60) * 60
;;

(* Do the main thing *)
let main() =
	(* print_string "Got thing.\n"; *)
	let rrd = (Array.get Sys.argv 1) in
	let stuff = { last_time = 0; total_calls = 0; total_time = 0 } in
	try
		while true do
			let l = (read_line()) in
			if is_log_entry(l) then
			begin
				let le = get_log_entry(l) in
				let t = get_approx_time(le) in

				(* Figure out if it's a good time to print out the entry *)
				if t <> stuff.last_time && stuff.last_time <> 0
					&& stuff.total_calls > 0
					then
				begin
					(* Print out the stuff *)
					print_string("update ");
					print_string(rrd);
					print_string(" ");
					print_int(stuff.last_time); print_string(":");
					print_int(stuff.total_calls); print_string(":");
					print_int(stuff.total_time); print_newline();

					stuff.total_calls <- 0;
					stuff.total_time <- 0;
				end;

				(* If there were calls, add them up *)
				if le.calls > 0 then
				begin
					stuff.last_time <- t;
					stuff.total_calls <- (stuff.total_calls + 1);
					stuff.total_time <- (stuff.total_time +
										(le.calltime / le.calls));
				end
			end;
		done;
	with End_of_file -> ignore();
;;

if !Sys.interactive then () else begin main() end;;
