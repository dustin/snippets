(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 16771F7A-3781-11D8-88E8-000393CB0F1E
 *)

(** Produce a hardlink copy of a tree. *)

(* Chop out the source dir part *)
let rel fn a =
	String.sub fn (String.length a) (String.length fn - String.length a)
;;

let linkto basedir d l a =
	List.iter (fun fn ->
		let srcfn = (Filename.concat d fn) in
		let destfn = (Filename.concat basedir (rel srcfn a)) in
		Printf.printf "ln %s -> %s\n" srcfn destfn;
		Fileutils.mkdirs 0o755 (Filename.dirname destfn);
		Unix.link srcfn destfn
	) l
;;

let main =
	(* This will return a (unit -> unit) *)
	Unix.handle_unix_error (fun () ->
		Fileutils.walk_dir Sys.argv.(1) (linkto Sys.argv.(2)) Sys.argv.(1))
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;
