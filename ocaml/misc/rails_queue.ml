(*
 * Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
 *)

let main () =
	let bs = Beanstalk.connect Sys.argv.(1) 11300 in
	ignore(Beanstalk.put bs 65535 0 3600 (
		Printf.sprintf "---\r\n:type: :rails\r\n:code: %s\r\n" Sys.argv.(2)
		));
	Beanstalk.shutdown bs
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end

