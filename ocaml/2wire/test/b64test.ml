(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 565F9316-295B-11D8-8F80-000393CB0F1E
 *)

let sc = Extstream.stream_convert Stream.of_string;;

let main() =
	(* Print out the base64 encoded first argument *)
	Fileutils.operate_on_file_in (fun f ->
			Stream.iter print_string (Base64.encode (Stream.of_channel f)))
		Sys.argv.(1);
	print_newline();
	(* Now try a file copy filtering through base64 *)
	let out = open_out Sys.argv.(2) in
	Fileutils.operate_on_file_in (fun f ->
				Stream.iter (fun s -> output_string out s)
			(Base64.decode (sc (Base64.encode (Stream.of_channel f)))))
		Sys.argv.(1);
	close_out out;
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end;;

