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

let block_same n data digest old_digest =
	()
;;

exception Subprocess_error of int;;

let block_diff prog n data digest old_digest =
	Printf.printf "Difference at block %d - %s != %s\n" n digest old_digest;
	let args = (Array.of_list
		["BLOCK=" ^ (string_of_int n);"DIGEST=" ^ digest]) in
	let pout,pin,perr = Unix.open_process_full prog args in
	ignore(output pin data 0 (String.length data));
	print_endline("Waiting...");
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
	Fileutils.operate_on_file_in
		(do_blocks blocksize bs bd (open_in oldmd5) newdigestch) Sys.argv.(1);
	close_out newdigestch;
	Unix.rename tmpmd5 oldmd5
;;

(**
 Args:  input_file digest_file diff_cmd
 *)
let main() =
	let newfn = (Sys.argv.(2) ^ ".new") in
	let oldfn = Sys.argv.(2) in
	process (8 * 1024) block_same (block_diff Sys.argv.(3))
		Sys.argv.(1) oldfn newfn
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;
