(* Copyright (c) 2002  Dustin Sallings <dustin@spy.net> *)
(* 2wire SQL Log parser *)

open Unix
open Extstring
open Fileutils

(** Log entry representing a single line from the file *)
type log_entry = {
	server: string;
	time: float;
	calls: int;
	calltime: int;
}

type stats = {
	db_server: string;
	mutable last_time: Nativeint.t;
	mutable total_calls: int;
	mutable total_time: int;
}

(* Parse time from the given timestamp *)
let parse_time l =
	let times = split_chars l [' '; ':'; '-'] 7 in
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
		})

(* Log entry printer *)
let print_log_entry(l: log_entry) =
	print_string(l.server);
	print_string(" ");
	print_string(Nativeint.to_string (Nativeint.of_float l.time));
	print_string(" ");
	print_int(l.calls);
	print_string(" ");
	print_int(l.calltime);
	print_newline()

(* Get a log entry from the line *)
let get_log_entry(l: string): log_entry =

	let parts = split l ' ' 12 in
	let tparts = split (List.nth parts 10) '/' 3 in
		{
			(* line_array[7] *)
			server = List.nth parts 7;
			(* line[0:19] *)
			time = parse_time(String.sub l 0 19);
			(* line_array[10][1] *)
			calls = int_of_string(List.nth tparts 1);
			(* line_array[10][0] *)
			calltime =
				int_of_string(String.sub (List.hd tparts) 0
					((String.length (List.hd tparts)) - 2))
		}

(* Round to the nearest minute *)
let get_approx_time le =
	let sixty = Nativeint.of_int 60 in
	Nativeint.mul sixty (Nativeint.div (Nativeint.of_float le.time) sixty)

(* Create a stat for each name in the names array *)
let make_stats(name: string): stats =
	{ db_server = name;
		last_time = Nativeint.zero;
		total_calls = 0;
		total_time = 0 }

(* Process the line *)
let process_entry rrd_prefix stuff le =
	let t = get_approx_time(le) in

	(* print_log_entry(le); *)

	(* Figure out if it's a good time to print out the entry *)
	if t <> stuff.last_time
		&& stuff.last_time <> Nativeint.zero
		&& stuff.total_calls > 0
		then
	begin
		(* Print out the stuff *)
		print_string("update ");
		print_string(rrd_prefix);
		print_string("-");
		print_string(le.server);
		print_string(".rrd");
		print_string(" ");
		print_string(Nativeint.to_string stuff.last_time); print_string(":");
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

let error_line(s: string) =
	prerr_string("Error on line:  ");
	prerr_endline(s)

(* Process an individual line *)
let process_line rrd servers l =
	try
		let le = get_log_entry(l) in
		let stuff = List.find (fun n -> n.db_server = le.server) servers in
		process_entry rrd stuff le;
	with
		Failure("int_of_string") -> error_line(l);
		| Failure("nth") -> error_line(l);
		| Not_found -> error_line(l)

(* Do the main thing *)
let main() =
	let rrd = (Array.get Sys.argv 1)
	and servers = List.map (fun x -> make_stats x) ["CMS";"LOG";"CAT"] in
	conditional_iter_lines
		(fun l -> process_line rrd servers l)
		(fun l -> String.length l > 60
						&& ((String.sub l 36 22) = "database.DBManager.sql"))
		Pervasives.stdin
;;

if !Sys.interactive then () else begin main() end
