(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: D336A06C-0A9C-11D8-988A-000393CFE6B8
 *)

(** Functions for processing lines of files. *)

(**
 Apply the given function to each line in the giving channel.

 @param f the function to apply to each line
 @param ch the in_channel to read
*)
let fold_lines f ch =
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
let conditional_fold_lines f c ch =
	fold_lines (function x -> if c x then f x) ch;
;;
