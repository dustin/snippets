(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

open Unix

(** Functions for processing files. *)

(** {1 Functions for processing lines of files} *)

(**
 Kind of like really_input, but returns as much as possible at EOF, as much as
 requested otherwise.

 @param ch the input channel
 @param buf the buffer into which to read
 @param pos the position to begin writing in the buffer
 @param len the amount to read

 @return the number of bytes read
 *)
let input_block ch buf pos len =
	let rec loop remaining =
		if remaining = 0 then (
			len
		) else (
			let rv = input ch buf (pos + (len - remaining)) remaining in
			if rv = 0 then (
				len - remaining
			) else (
				loop (remaining - rv)
			)
		)
	in
	loop len

(**
 Pervasives.open that will open ``-'' as stdin
 *)
let my_open fn =
	if fn = "-" then Pervasives.stdin
	else Pervasives.open_in fn


(**
 Apply the given function to each line in the giving channel.

 @param f the function to apply to each line
 @param ch the in_channel to read
*)
let iter_lines f ch =
	let rec loop() =
		f (input_line ch);
		loop() in
	try
		loop()
	with End_of_file -> ()

(**
 Return the value of f called on each line of input along with the current
 value (calculated from init).  (... (f input (f input init) ...) until EOF.

 @param f the function to call on each line with the current fold value
 @param ch the channel to read
 @param init_value the initial value for the fold
 *)
let rec fold_lines f init_value ch =
	let line,eof = try (input_line ch), false
		with End_of_file -> "", true
	in
	if eof then
		init_value
	else
		fold_lines f (f line init_value) ch

(**
 Apply the given function to each line in the giving channel if the function
 c returns true for the line.

 @param f the function to apply to each line
 @param c the condition to test on the line
 @param ch the in_channel to read
*)
let conditional_iter_lines f c ch =
	iter_lines (function x -> if c x then f x) ch

(**
	Open a file for reading, and perform the given operation on it.
	The file is guaranteed to be closed when this function completes.

	@param f the function to perform with the input channel
	@param fn the name of the file to open
*)
let operate_on_file_in f fn =
	let ch = my_open fn in
	try
		let rv = f ch in
		close_in ch;
		rv
	with x ->
		close_in ch;
		raise x

(**
 	Open a file for writing and perform the given operations on it.
	The file is guaranteed to be closed when this function completes.

	@param f the function to perform with the output channel
	@param fn the name of the file to open
 *)
let operate_on_file_out f fn =
	let ch = open_out fn in
	try
		let rv = f ch in
		close_out ch;
		rv
	with x ->
		close_out ch;
		raise x

(** Open a file for reading and iterate the lines.

  @param f the function to be called on each line
  @param fn the name of the file to operate on
*)
let iter_file_lines f fn =
	operate_on_file_in (iter_lines f) fn

(** Open a file for reading and iterate the lines.

  @param f the function to be called on each line
  @param init_value the initial value for the fold
  @param fn the name of the file to operate on
*)
let fold_file_lines f init_value fn =
	operate_on_file_in (fold_lines f init_value) fn

(** {1 Functions for processing directories} *)

(** Ensure the given path exists.  (make all directories up to this path) *)
let rec mkdirs perm path =
	try
		Unix.access path [Unix.F_OK]
	with Unix.Unix_error (e,i,o) ->
		mkdirs perm (Filename.dirname path);
		Unix.mkdir path perm

(**
 Debug routine to pass to a directory folding routine.
 *)
let debug_dir_print d l a =
	print_endline("Iterating " ^ d);
	List.iter (fun x -> print_endline("\t" ^ (Filename.concat d x))) l

(**
 Private recursive routine for creating a list from readdir.
 *)
let rec pvt_lsdir d a =
	try
		pvt_lsdir d ((readdir d) :: a)
	with End_of_file -> a

(**
 Get a list of files in a directory (excluding . and ..).

 @param dn the name (path) of the directory
 *)
let lsdir dn =
	let d = opendir dn in
	let rv = List.filter (fun x -> not (x = "." || x = ".."))
							(pvt_lsdir d []) in
	closedir d;
	rv

(** Stat cache *)
let stat_func = ref Unix.stat

(** Set the stat function (allowing for caching or whatever) *)
let set_stat_func f = stat_func := f

(**
 Is this path a directory?

 @param p the path to check
 *)
let isdir p =
	(!stat_func p).st_kind = S_DIR

(**
 Iterate all of the files in a directory via a given directory listing
 function.

 @param lsfunc the name of the function which will return a list of filenames
 	from a given directory
 @param dir the name of the directory to process
 @param func a function that will receive the directory name, list of filenames
 	and an argument
 @param arg an arbitrary argument that will be passed to the function
 *)
let dir_iter_via lsfunc dir func arg =
	func dir (lsfunc dir) arg

(**
 Iterate all of the files in a directory.

 @param dir the name of the directory to process
 @param func a function that will receive the directory name, list of filenames and an argument
 @param arg an arbitrary argument that will be passed to the function
 *)
let dir_iter dir func arg =
	func dir (lsdir dir) arg

(**
 Walk a directory tree (depth first).
 
 @param lsfunc the function to list files in a directory
 @param dir the directory to search
 @param func function to call (see fold_directory)
 @param arg the argument to pass to the function
 *)
let rec walk_dir_via lsfunc dir func arg =
	dir_iter_via lsfunc dir
		(fun d l a ->
			(* Split into dirs and files *)
			let (dirs, files) =
				List.partition (fun x -> isdir (Filename.concat d x)) l in
			(* Recurse through the dirs *)
			List.iter (fun x -> walk_dir_via lsfunc
							(Filename.concat d x) func arg) dirs;
			(* And then process the files *)
			func d files arg)
	arg

(**
 Walk a directory tree (depth first).
 
 @param dir the directory to search
 @param func function to call (see fold_directory)
 @param arg the argument to pass to the function
 *)
let rec walk_dir dir func arg =
	dir_iter dir
		(fun d l a ->
			(* Split into dirs and files *)
			let (dirs, files) =
				List.partition (fun x -> isdir (Filename.concat d x)) l in
			(* Recurse through the dirs *)
			List.iter (fun x -> walk_dir (Filename.concat d x) func arg) dirs;
			(* And then process the files *)
			func d files arg)
	arg
