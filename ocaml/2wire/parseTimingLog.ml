(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 6BF17402-05DC-11D8-BEE8-000393DC8AE4
 *)

open Unix;;
open Hashtbl;;
open List;;
open Extstring;;
open Fileutils;;

(* This exception is thrown when we go back in time *)
exception Back_in_time of string;;

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

type global_state_t = {
	mutable g_last_ts: Nativeint.t;
	mutable g_blocks: (string * per_block) list;
};;

(* The types of logs we consider *)
let log_types = ["HB"; "BOOT"; "KICK"; "XMLRPC";];;

(* Log times with time/count/start/end appended *)
let extended_log_types =
	List.concat (List.map (fun x ->
		(List.map (fun y -> x ^ y) ["time";"count";"start";"end"]))
		log_types)
;;

(* Stringify a log entry *)
let string_of_log_entry le =
	(string_of_float le.le_time) ^ ":" ^ le.le_serial ^ " " ^ le.le_ttype
		^ " " ^ le.le_state
;;

(* Get the block that contains the counts for the given log entry.
 * This will also create any tables it needs.
 *)
let empty_block =
	(* If we didn't have a match, build it all out *)
	List.map (fun x -> (x, {
						pb_time = 0.0;
						pb_count = 0;
						pb_start = 0;
						pb_end = 0;
					})) log_types
;;

let global_state = {g_last_ts = Nativeint.zero; g_blocks = empty_block };;

(* Reset all of the globals for a new timestamp *)
let reset_global ts =
	global_state.g_last_ts <- ts;
	List.iter (fun (_, x) ->
			x.pb_time <- 0.0;
			x.pb_count <- 0;
			x.pb_start <- 0;
			x.pb_end <- 0) global_state.g_blocks
;;

(* This is the hashtable that will hold the state and stuff *)
let eventCache=Hashtbl.create 1;;

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

(* Time approximation function *)
let approx_time t =
	let sixty = Nativeint.of_int 60 in
	Nativeint.mul sixty (Nativeint.div (Nativeint.of_float t) sixty)
;;

(* Print the header for a block *)
let make_block_header filename =
	"update " ^ filename ^ " -t "
	^ (String.concat ":" extended_log_types)
	^ " "
;;

let make_entry fn =
	(make_block_header fn)
	^ (Nativeint.to_string global_state.g_last_ts)
	^ ":"
    ^ (String.concat ":"
        (List.concat
            (List.map (fun (_, pb) ->
                [string_of_float pb.pb_time;
                string_of_int pb.pb_count;
                string_of_int pb.pb_start;
                string_of_int pb.pb_end; ]) global_state.g_blocks)))
;;

(* Output the current data *)
let print_entry fn =
	print_endline(make_entry fn);
	(* prerr_endline(make_entry fn); *)
;;

(* Get a log timing record for recording the state between the two entries *)
let get_log_timing le1 le2 =
	{
		lt_start = le1.le_time;
		lt_stop = le2.le_time;
		lt_ttype = le1.le_ttype;
		lt_serial = le1.le_serial;
	}
;;

(* Record the timing for this mofo *)
let record_timing le_old le_new li =
	let lt = get_log_timing le_old le_new in
	li.pb_count <- li.pb_count + 1;
	li.pb_time <- li.pb_time +. (lt.lt_stop -. lt.lt_start);
	();
;;

(* Process a given log entry *)
let process le rrd =
	(* first check to see if we need to print stuff *)
	let at = approx_time le.le_time in
	if at <> global_state.g_last_ts then
		begin
			(* If we see a log entry that is older than the current set,
				there's a problem. *)
			if at < global_state.g_last_ts then
				raise (Back_in_time (string_of_log_entry le));
			(* If this is not the first one, print it out *)
			if global_state.g_last_ts <> Nativeint.zero then
				print_entry rrd;
			(* Adjust all of the global params *)
			reset_global at
		end;

	(* Grab the type block and process the start or end *)
	let it = List.assoc le.le_ttype global_state.g_blocks in
	match le.le_state with
	| "start" ->
		begin
			it.pb_start <- it.pb_start + 1;
			try
				Hashtbl.find eventCache le.le_serial;
				prerr_endline("Duplicate start for " ^ le.le_serial);
				Hashtbl.remove eventCache le.le_serial;
			with Not_found -> ();
			Hashtbl.add eventCache le.le_serial le;
		end;
	| "end" ->
		begin
			it.pb_end <- it.pb_end + 1;
			try
				let old = Hashtbl.find eventCache le.le_serial in
				record_timing old le it;
				Hashtbl.remove eventCache le.le_serial;
			with Not_found ->
				prerr_endline("No start for end " ^ le.le_serial);
		end;
	| _ -> raise (Invalid_argument le.le_state);
;;

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

(* do it *)
let main() =
	let rrd = (Array.get Sys.argv 1) in
	conditional_iter_lines
		(fun l ->
			try
				let le = get_log_entry l in
				try
					process le rrd;
				with
					Not_found -> prerr_endline("Type not found:  "
						^ le.le_ttype);
			with x ->
				prerr_endline("Unknown error on " ^ l);
				prerr_endline(Printexc.to_string x);
			)
		(fun l -> (strstr l "TransactionTiming" 40) >= 40)
		Pervasives.stdin;
	(* Don't forget the last line *)
	print_entry rrd
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;

