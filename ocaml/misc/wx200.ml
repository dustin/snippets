(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 68B449BA-3392-11D8-8674-000393CB0F1E
 *)

open Unix

type package_t = Time_humidity | Temp | Barometer_dew | Rain | Wind | Time

type temp_reading = {
	outdoor_temp: float;
	indoor_temp: float;
}

type time_type = {
	time_hr: int;
	time_min: int;
	time_sec: int;
}

type hum_type = {
	outdoor_hum: int;
	indoor_hum: int;
}

type prediction = Sunny | Cloudy | Partly_cloudy | Rainy

type barometer_trends = Rising | Steady | Falling

type bar_type = {
	pred: prediction;
	bar_trend: barometer_trends;
	pressure: int;
	in_dew: int;
	out_dew: int;
}

type reading = None
	| Temp_reading of temp_reading
	| Rain_reading of int
	| Wind_reading of int
	| Time_reading of time_type
	| Hum_reading of hum_type
	| Bar_reading of bar_type

let construct_type = function
	  0x8f -> Time_humidity
	| 0x9f -> Temp
	| 0xaf -> Barometer_dew
	| 0xbf -> Rain
	| 0xcf -> Wind
	| 0xff -> Time
	| n -> failwith("Unknown type: " ^ (string_of_int n))

let njunk s n =
	List.iter (fun _ -> Stream.junk s) (Stream.npeek n s)

let nget s n =
	let rv = (Stream.npeek n s) in
	List.iter (fun _ -> Stream.junk s) rv;
	rv

let print_reading = function
	  Temp_reading tr ->
		Printf.printf "Temp reading:  in=%f out=%f\n"
			tr.indoor_temp tr.outdoor_temp
	| Rain_reading rr ->
		Printf.printf "Rain reading:  %dmm\n" rr
	| Wind_reading wr ->
		Printf.printf "Wind reading:  %dmps\n" wr
	| Time_reading tr ->
		(*
		Printf.printf "Time reading:  %02d:%02d:%02d\n"
			tr.time_hr tr.time_min tr.time_sec
		*)
		()
	| Hum_reading hr ->
		Printf.printf "Humidity reading:  in=%d%% out=%d%%\n"
			hr.indoor_hum hr.outdoor_hum
	| Bar_reading br ->
		Printf.printf "Barometer reading:  %dMBAR (%s) - Prediction: %s\n"
			br.pressure
			(match br.bar_trend with
				Rising -> "rising"
				| Steady -> "steady"
				| Falling -> "falling")
			(match br.pred with
				Sunny -> "sunny"
				| Cloudy -> "cloudy"
				| Partly_cloudy -> "partly cloudy"
				| Rainy -> "rainy");
		Printf.printf "\tDew points:  indoor=%d outdoor=%d\n"
			br.in_dew br.out_dew
	| _ ->
		print_endline("Unhandled print type")

let bcd_int i =
	(((i land 0xf0) lsr 4) * 10) + (i land 0x0f)

let decode_temp i1 i2 =
	let bc = (float_of_int (bcd_int i1)) in
	let a = (float_of_int (i2 land 0x7)) in
	let d = (a *. 10.0) +. (bc /. 10.0) in
	if (i2 land 0x8) = 0 then
		d
	else
		0.0 -. d

let decoder s =
	let first_byte = Stream.next s in
	match construct_type(int_of_char first_byte) with
		Time_humidity ->
			let bytes = List.map int_of_char (nget s 34) in
			Hum_reading {
				indoor_hum = bcd_int (List.nth bytes 7);
				outdoor_hum = bcd_int (List.nth bytes 19)
			}
		| Temp ->
			let bytes = List.map int_of_char (nget s 33) in
			Temp_reading {
				indoor_temp =
					decode_temp (List.nth bytes 0) (List.nth bytes 1);
				outdoor_temp =
					decode_temp (List.nth bytes 15) (List.nth bytes 16);
			}
		| Barometer_dew ->
			let bytes = List.map int_of_char (nget s 30) in
			Bar_reading ( {
				bar_trend=(match ((0x70 land (List.nth bytes 5)) lsr 4) with
					1 -> Falling
					| 2 -> Steady
					| 4 -> Rising
					| x -> failwith("Invalid bar trend: " ^ (string_of_int x)));
				pred=(match (0x0f land (List.nth bytes 5)) with
					1 -> Sunny
					| 2 -> Cloudy
					| 4 -> Partly_cloudy
					| 8 -> Rainy
					| x -> failwith("Invalid prediction: "
						^ (string_of_int x)));
				pressure=((bcd_int (List.nth bytes 1)) * 100)
					+ (bcd_int (List.nth bytes 0));
				in_dew = (bcd_int (List.nth bytes 6));
				out_dew = (bcd_int (List.nth bytes 17));
			} )
		| Rain ->
			let bytes = List.map int_of_char (nget s 13) in
			Rain_reading (
				(bcd_int (List.nth bytes 0)) +
				(100 * (bcd_int (0xf land (List.nth bytes 1))))
			)
		| Wind ->
			let bytes = List.map int_of_char (nget s 26) in
			Wind_reading (
				(bcd_int (List.nth bytes 0)) +
				(100 * (bcd_int (0xf land (List.nth bytes 1))))
			)
		| Time ->
			let bytes = List.map int_of_char (nget s 4) in
			Time_reading (
				{time_hr = bcd_int (List.nth bytes 2);
				 time_min = bcd_int (List.nth bytes 1);
				 time_sec = bcd_int (List.nth bytes 0); })

let rec main_loop s =
	print_reading (decoder s);
	flush Pervasives.stdout;
	main_loop s

let lookup h =
	let he = gethostbyname h in
	he.h_addr_list.(0)

let main() =
	let s = socket PF_INET SOCK_STREAM 0 in
	connect s (ADDR_INET (lookup "juan", 9753));
	main_loop (Stream.of_channel (in_channel_of_descr s))
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
