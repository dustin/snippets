(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 74C885B4-0F5E-11D8-A83D-000393CFE6B8
 *)

open Unix;;
open Char;;

let string_of_bool b = if b then "true" else "false";;

let string_of_char c = escaped c;;

let printer a b =
	print_string(a);
	print_string(" = ");
	print_string(b);
	print_newline();
;;

let main() =
	let tio = tcgetattr Unix.stdin in
	printer "ignbrk" (string_of_bool tio.c_ignbrk);
	printer "brkint" (string_of_bool tio.c_brkint);
	printer "ignpar" (string_of_bool tio.c_ignpar);
	printer "parmrk" (string_of_bool tio.c_parmrk);
	printer "inpck " (string_of_bool tio.c_inpck);
	printer "istrip" (string_of_bool tio.c_istrip);
	printer "inlcr " (string_of_bool tio.c_inlcr);
	printer "igncr " (string_of_bool tio.c_igncr);
	printer "icrnl " (string_of_bool tio.c_icrnl);
	printer "ixon  " (string_of_bool tio.c_ixon);
	printer "ixoff " (string_of_bool tio.c_ixoff);
	printer "opost " (string_of_bool tio.c_opost);
	printer "obaud " (string_of_int tio.c_obaud);
	printer "ibaud " (string_of_int tio.c_ibaud);
	printer "csize " (string_of_int tio.c_csize);
	printer "cstopb" (string_of_int tio.c_cstopb);
	printer "cread " (string_of_bool tio.c_cread);
	printer "parenb" (string_of_bool tio.c_parenb);
	printer "parodd" (string_of_bool tio.c_parodd);
	printer "hupcl " (string_of_bool tio.c_hupcl);
	printer "clocal" (string_of_bool tio.c_clocal);
	printer "isig  " (string_of_bool tio.c_isig);
	printer "icanon" (string_of_bool tio.c_icanon);
	printer "noflsh" (string_of_bool tio.c_noflsh);
	printer "echo  " (string_of_bool tio.c_echo);
	printer "echoe " (string_of_bool tio.c_echoe);
	printer "echok " (string_of_bool tio.c_echok);
	printer "echonl" (string_of_bool tio.c_echonl);
	printer "vintr " (string_of_char tio.c_vintr);
	printer "vquit " (string_of_char tio.c_vquit);
	printer "verase" (string_of_char tio.c_verase);
	printer "vkill " (string_of_char tio.c_vkill);
	printer "veof  " (string_of_char tio.c_veof);
	printer "veol  " (string_of_char tio.c_veol);
	printer "vmin  " (string_of_int tio.c_vmin);
	printer "vtime " (string_of_int tio.c_vtime);
	printer "vstart" (string_of_char tio.c_vstart);
	printer "vstop " (string_of_char tio.c_vstop);
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;
