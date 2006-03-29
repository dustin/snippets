(*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: dustin_20040801T214918_644
 *)

open Unix

(* stat cache *)
let stat_cache = Lru.create_auto 8192 Unix.stat;;
let stat = Lru.find stat_cache;;
Fileutils.set_stat_func stat

let md5_file f =
	Digest.to_hex (Digest.file f)

let process_dir d l a =
	List.iter (fun fn ->
		Printf.printf "%s\t%s\n" (md5_file fn) fn)
		(List.map (fun x -> Filename.concat d x) l)

let walker dir =
	Fileutils.walk_dir dir process_dir ()

let main () =
	Arg.parse [
	] walker
	"Walk the given files or directories looking for stuff to digest"
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
