(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 1E4EFAC0-498E-11D8-A07A-000A957659CC
 *)

(**
 Calculate a digest for each block in a file.
 *)

(**
 Get the string representation of the digest of the current block.
 *)
let digest_of_block s len =
	Digest.to_hex (Digest.substring s 0 len)
;;

(**
 Get the previously stored digest.
 *)
let get_old_digest ch =
	try
		input_line ch
	with End_of_file -> ""
;;

(**
 Deal with the blocks.

 @param bs function to call when the blocks are the same
 @param bd function to call when the blocks differ
 @param digestin the input digest file
 @param digestout the output (new) digest file
 @param ch the input stream we're comparing
 *)
let do_blocks blocksize bs bd digestin digestout ch =
	(* Eat the first line of the oldmd5 file (meta info) *)
	ignore(get_old_digest digestin);
	let buf = String.create blocksize in
	let rec loop n =
		let bytesread = Fileutils.input_block ch buf 0 blocksize in
		let old_digest = get_old_digest digestin in
		let new_digest = digest_of_block buf bytesread in
		(* Let the caller know whether it's the same or different *)
		(if new_digest = old_digest then bs else bd) n
			(String.sub buf 0 bytesread) new_digest old_digest;
		(* Record the new digest *)
		output_string digestout (new_digest ^ "\n");
		(* If we didn't read EOF, do it again *)
		if bytesread = blocksize then (
			loop (succ n)
		)
		in
	loop 0
;;

let makefn = Printf.sprintf "%s/%05d";;

let block_same prevdir newdir n data digest old_digest =
	let oldpath = makefn prevdir n in
	let newpath = makefn newdir n in
	(* print_endline ("Linking " ^ oldpath ^ " -> " ^ newpath); *)
	Unix.link oldpath newpath
;;

exception Subprocess_error of int;;

let block_diff prevdir newdir prog n data digest old_digest =
	(*
	Printf.printf "Difference at block %d - %s != %s\n" n digest old_digest;
	*)
	let args = (Array.of_list
		["BLOCK=" ^ (string_of_int n);
		 "DIGEST=" ^ digest;
		 "PREVDIR=" ^ prevdir;
		 "NEWDIR=" ^ newdir;
		 "PREVPATH=" ^ (makefn prevdir n);
		 "NEWPATH=" ^ (makefn newdir n)]) in
	(* Open the subprocess *)
	let pout,pin,perr = Unix.open_process_full prog args in
	(* Send the data to the subprocess *)
	output pin data 0 (String.length data);
	(* Check the result of the subprocess *)
	(match Unix.close_process_full(pout,pin,perr) with
		Unix.WEXITED v ->
			if v != 0 then raise (Subprocess_error v)
		| Unix.WSIGNALED v -> raise (Subprocess_error v)
		| Unix.WSTOPPED v -> raise (Subprocess_error v)
	)
;;

(**
 Process the given files.

 @param blocksize digest block size
 @param bs function to call when the blocks are the same
 @param bd function to call when the blocks are different
 @param datafn name of the file containing the data (may be - for stdin)
 @param oldmd5 location of the file containing existing md5s
 @param tmpmd5 location of the tmp file that will contain the new md5s
 *)
let process blocksize bs bd datafn oldmd5 tmpmd5 =
	let newdigestch = (open_out tmpmd5) in
	(* Write out the block size on the first line *)
	output_string newdigestch ("# block size:  "
		^ (string_of_int blocksize) ^ "\n");
	Fileutils.operate_on_file_in
		(do_blocks blocksize bs bd (open_in oldmd5) newdigestch) Sys.argv.(1);
	close_out newdigestch;
	Unix.rename tmpmd5 oldmd5
;;

(**
 Args:  input_file digest_file diff_cmd
 *)
let main() =
	(* Arg checking *)
	if (Array.length Sys.argv) < 6 then (
		prerr_endline("Usage:  " ^ Sys.argv.(0)
			^ " streamfn digestfile diffscript prevdir newdir [blocksize]");
		exit(1)
	);
	let blocksize = ref (8 * 1024 * 1024) in
	if (Array.length Sys.argv) > 6 then (
		blocksize := (int_of_string Sys.argv.(6))
	);
	if !blocksize > Sys.max_string_length then (
		blocksize := Sys.max_string_length
	);
	(* Pop off the args *)
	let streamfn = Sys.argv.(1) in
	let oldfn = Sys.argv.(2) in
	let newfn = (oldfn ^ ".new") in
	let scriptname = Sys.argv.(3) in
	let prevdir = Sys.argv.(4) in
	let newdir = Sys.argv.(5) in
	process !blocksize
		(block_same prevdir newdir)
		(block_diff prevdir newdir scriptname)
		streamfn oldfn newfn
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;
