(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: E84FA9D5-102C-11D8-9148-000393CFE6B8
 *)

open Fileutils;;
open Filename;;
open String;;
open Stringutils;;
open Unix;;
open Gz;;

let gz_magic="\031\139";;

let max_age = 2.0 *. 86400.0;;

let buf_size = 8192;;

let start_time = Unix.time();;

(* True if this is a gzip file *)
let is_gzip fn =
	(* print_endline("Checking gzipness of " ^ fn); *)
	let buf = "12"
	and f = open_in_bin fn in
	let nread = input f buf 0 2 in
	let rv = (nread = 2 && buf = gz_magic) in
	Pervasives.close_in f;
	rv
;;

(* Get the age of the given file *)
let age_of fn =
	start_time -. (stat fn).st_mtime
;;

(* True if this file is new enough for processing *)
let is_new_enough fn =
	(age_of fn) < max_age
;;

let should_process fn =
	try
		(ends_with fn ".") && (is_new_enough fn) && (not (is_gzip fn))
	with x ->
		print_endline("Unknown error determining whether to process " ^ fn);
		raise x
;;

let rec copy_gzip infile gzfile buf =
	let nread = input infile buf 0 buf_size in
	if nread > 0 then
	begin
		Gz.write gzfile ~buf:buf ~pos:0 ~len:nread;
		copy_gzip infile gzfile buf;
	end;
	()
;;

let compress_file srcfile destfile =
	let gzfile = Gz.open_out ~compression:9 ~strategy:Default destfile
	and infile = open_in_bin srcfile
	and buf = String.create buf_size in
	copy_gzip infile gzfile buf;
	Pervasives.close_in infile;
	Gz.close_out gzfile;
;;

(* Get a temporary name for the gzip destination *)
let get_tmp_name d f =
	f ^ ".gztmp"
;;

(* Process a file in the given directory *)
let process_file d f =
	print_endline("Processing " ^ f);
	let tmpname = get_tmp_name d f in
	try
		compress_file f tmpname;
		rename tmpname f;
	with x ->
		unlink tmpname;
		match x with
			| (Gz.Error s) -> print_endline("Gzip error on " ^ f ^ ": " ^ s);
			| _ ->
				print_endline("Unknown error on " ^ f);
				raise x
;;

let process_dir d l a =
	List.iter (fun fn ->
		if (should_process fn) then (process_file d fn))
		(List.map (fun x -> Filename.concat d x) l)
;;

let main () =
	List.iter (fun d -> walk_dir d process_dir () )
		(List.tl (Array.to_list Sys.argv))
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;

