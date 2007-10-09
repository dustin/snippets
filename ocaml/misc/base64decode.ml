(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

let sc = Extstream.stream_convert Stream.of_string

let main() =
	(* Print out the base64 encoded first argument *)
	Fileutils.operate_on_file_in (fun f ->
			Stream.iter print_string (Base64.decode (Stream.of_channel f)))
		Sys.argv.(1);
	print_newline()
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
