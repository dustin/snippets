(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 565F9316-295B-11D8-8F80-000393CB0F1E
 *)

let main() =
	Fileutils.operate_on_file (fun f ->
			Stream.iter print_string
				(Stream.from (Base64.stream_encode (Stream.of_channel f))))
		Sys.argv.(1);
	print_newline()
;;

(* Start main if we're interactive. *)
if !Sys.interactive then () else begin main() end;;

