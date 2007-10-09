(*
 * Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
 *)

open Mfgcdb;;

let displayRecord record =
	Printf.printf "key:\t\t%d\n" record.gateway_key;
	Printf.printf "sn:\t\t``%s''\n" record.sn;
	Printf.printf "id:\t\t``%s''\n" record.id_string;
	Printf.printf "pca:\t\t``%s''\n" record.pca;
	Printf.printf "model:\t\t``%s''\n" record.model_num;
	Printf.printf "product_string:\t``%s''\n" record.product_string;
	Printf.printf "version:\t``%s''\n" record.version;
	Printf.printf "auth_code:\t``%s''\n" record.auth_code;
	Printf.printf "ssid:\t\t``%s''\n" record.ssid;
	Printf.printf "date_mod:\t``%s''\n" record.date_mod;
	Printf.printf "wireless_id:\t``%s''\n" record.wireless_id;
	Printf.printf "-----------\n%!"

let rec showKeys mfgcdb = function
	  [] -> ()
	| key::rest ->
		let record = lookup mfgcdb key in
		displayRecord record;
		showKeys mfgcdb rest

let usage() =
	Printf.eprintf "Usage:  %s cdbfile key [key...]\n" Sys.argv.(0);
	exit 1

let main () =
	match (List.tl (Array.to_list Sys.argv)) with
	  [] -> usage()
	| file::keys ->
		let mfgcdb = open_mfg_db file in
		showKeys mfgcdb keys;
		close_mfg_db mfgcdb
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
