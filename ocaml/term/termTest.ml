(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 74C885B4-0F5E-11D8-A83D-000393CFE6B8
 *)

open Unix;;

(* Boolean printer *)
let print_bool b = print_string(if b then "true" else "false");;

let print_poly x = match x with
	| (x: bool) -> print_bool x
;;

let printer a b =
	print_string(a);
	print_string(" = ");
	print_poly(b);
	print_newline();
;;

let main() =
	let tio = tcgetattr Unix.stdin in
	printer "ignbrk" tio.c_ignbrk;
	printer "brkint" tio.c_brkint;
	printer "ignpar" tio.c_ignpar;
	printer "parmrk" tio.c_parmrk;
	printer "inpck " tio.c_inpck;
	printer "istrip" tio.c_istrip;
	printer "inlcr " tio.c_inlcr;
	printer "igncr " tio.c_igncr;
	printer "icrnl " tio.c_icrnl;
	printer "ixon  " tio.c_ixon;
	printer "ixoff " tio.c_ixoff;
	printer "opost " tio.c_opost;
	(*
	printer "obaud " tio.c_obaud;
	*)
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;
