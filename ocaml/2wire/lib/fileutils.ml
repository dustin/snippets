(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: D336A06C-0A9C-11D8-988A-000393CFE6B8
 *)

open Unix;;

(** Functions for processing files. *)

(** {1 Functions for processing lines of files} *)

(**
 Apply the given function to each line in the giving channel.

 @param f the function to apply to each line
 @param ch the in_channel to read
*)
let iter_lines f ch =
	try
		while true do
			let l = (input_line ch) in
			f l;
		done;
	with End_of_file -> ();
;;

(**
 Apply the given function to each line in the giving channel if the function
 c returns true for the line.

 @param f the function to apply to each line
 @param c the condition to test on the line
 @param ch the in_channel to read
*)
let conditional_iter_lines f c ch =
	iter_lines (function x -> if c x then f x) ch;
;;

(** {1 Functions for processing directories} *)

(**
 Debug routine to pass to a directory folding routine.
 *)
let debug_dir_print d l a =
	print_endline("Iterating " ^ d);
	List.iter (fun x -> print_endline("\t" ^ (Filename.concat d x))) l
;;

(**
 Private recursive routine for creating a list from readdir.
 *)
let rec pvt_lsdir d a =
	try
		pvt_lsdir d ((readdir d) :: a)
	with End_of_file -> a
;;

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
;;

(** Stat cache *)
let stat_func = ref Unix.stat;;

(** Set the stat function (allowing for caching or whatever) *)
let set_stat_func f = stat_func := f;;

(**
 Is this path a directory?

 @param p the path to check
 *)
let isdir p =
	(!stat_func p).st_kind = S_DIR
;;

(**
 Iterate all of the files in a directory.

 @param dir the name of the directory to process
 @param func a function that will receive the directory name, list of filenames and an argument
 @param arg an arbitrary argument that will be passed to the function
 *)
let dir_iter dir func arg =
	func dir (lsdir dir) arg
;;

(**
 Walk a directory tree.

 @param dir the directory to search
 @param func function to call (see fold_directory)
 @param arg the argument to pass to the function
 *)
let rec walk_dir dir func arg =
	dir_iter dir
		(fun d l a ->
			let fq = List.map (fun x -> Filename.concat d x) l in
			List.iter (fun x -> walk_dir x func arg)
				(List.filter isdir fq);
			func d (List.filter (fun x ->
				not (isdir (Filename.concat d x))) l) arg)
	arg
;;
