(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 771B6C22-2C66-11D8-AC9A-000393CB0F1E
 *)

open Unix;;

let lookup h =
	let he = gethostbyname h in
	he.h_addr_list.(0)
;;

let try_connect h p =
	let s = socket PF_INET SOCK_STREAM 0 in
	set_nonblock s;
	try connect s (ADDR_INET (lookup h, p)); s
	with x ->
		match x
		with Unix_error (e,fn,p) ->
			if(e != EINPROGRESS) then
				raise x;
			(* In progress is what we want *)
			s
		| _ -> raise x
;;

let rec main_loop h p =
	let s = try_connect h p in
	let r,w,e = select [s] [s] [s] 5.0 in
	let connected = (
		try
			if (List.length r) > 0 then (
				let buf = String.create 1 in
				ignore(read s buf 0 1);
				true
			) else if (List.length w) > 0 then (
				true
			) else (
				print_endline("Timed out");
				false
			)
		with Unix_error(e,_,_) ->
			print_endline(error_message e);
			false
	) in
	close s;
	if connected then (
		print_endline "Connected"
	) else (
		print_endline("Trying again...");
		sleep 1;
		main_loop h p;
	)
;;

let main() =
	if (Array.length Sys.argv) < 3 then (
		prerr_endline("Usage:  " ^ Sys.argv.(0) ^ " hostname port")
	) else (
		main_loop Sys.argv.(1) (int_of_string Sys.argv.(2))
	)
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

