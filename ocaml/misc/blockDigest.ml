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

(** Take a string and int and make a filename.
 e.g. ``x aabbcc 213'' -> x/aa/aabbcc.213 *)
let srcFnForBlock d s =
	Printf.sprintf "%s/%s/%s.%d" d (String.sub s 0 2) s

(** true if the given file exists *)
let exists f =
	try (Unix.access f [Unix.F_OK]; true) with _ -> false

let makefn = Printf.sprintf "%s/%05d"

(**
 Deal with the blocks.

 For every block, check to see if we have a file with the given md5 and size.
 If not, create one.  Then hard link the current filename to that file.

 @param blocksize the size of the blocks to read
 @param md5dir the directory containing the md5 blocks
 @param newdir the directory that will receive the new files
 @param bd function to call when a block is unknown
 @param ch the file being digested
 *)
let do_blocks blocksize md5dir newdir bd ch =
	let buf = String.create blocksize in
	let rec loop n =
		let bytesread = Fileutils.input_block ch buf 0 blocksize in
		let new_digest = digest_of_block buf bytesread in
		let srcFile = srcFnForBlock md5dir new_digest bytesread in
		(* Let the caller know whether it's the same or different *)
		if not (exists srcFile) then bd srcFile newdir n
			(String.sub buf 0 bytesread) new_digest;
		Unix.link srcFile (makefn newdir n);
		(* If we didn't read EOF, do it again *)
		if bytesread = blocksize then (
			loop (succ n)
		)
		in
	loop 0

exception Subprocess_error of int;;

let block_diff prog srcFile newdir n data digest =
	(*
	Printf.eprintf "Difference at block %d - %s != %s\n%!" n digest old_digest;
	*)
	let args = (Array.of_list
		["BLOCK=" ^ (string_of_int n);
		 "DIGEST=" ^ digest;
		 "NEWDIR=" ^ newdir;
		 "PREVPATH=" ^ srcFile;
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

(**
 Process the given files.

 @param blocksize digest block size
 @param bd function to call when the blocks are different
 @param datafn name of the file containing the data (may be - for stdin)
 *)
let process blocksize md5dir newdir bd datafn =
	Fileutils.operate_on_file_in
		(do_blocks blocksize md5dir newdir bd) Sys.argv.(1)

(**
 Args:  streamfn diffscript md5dir newdir [blocksize]
 *)
let main() =
	(* Arg checking *)
	if (Array.length Sys.argv) < 5 then (
		prerr_endline("Usage:  " ^ Sys.argv.(0)
			^ " streamfn diffscript md5dir newdir [blocksize]");
		exit(1)
	);
	let blocksize = ref (8 * 1024 * 1024) in
	if (Array.length Sys.argv) > 5 then (
		blocksize := (int_of_string Sys.argv.(5))
	);
	if !blocksize > Sys.max_string_length then (
		blocksize := Sys.max_string_length
	);
	(* Pop off the args *)
	let streamfn = Sys.argv.(1) in
	let scriptname = Sys.argv.(2) in
	let md5dir = Sys.argv.(3) in
	let newdir = Sys.argv.(4) in
	process !blocksize md5dir newdir (block_diff scriptname) streamfn
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;
