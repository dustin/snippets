(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

exception SubprocessError of string

(* Get a string from the tput command. *)
let tput x =
	let r = Unix.open_process_in ("tput " ^ x)
	and buf = String.create 8 in
	let len = input r buf 0 8 in
	let status = Unix.close_process_in r in
	begin
	match status with
		Unix.WEXITED(st) -> if (st != 0) then
						raise (SubprocessError "Non-zero exit status");
		| _ -> raise (SubprocessError "Unknown error.");
	end;
	String.sub buf 0 len

(* The two characters we need. *)
let smso = tput "smso"
let rmso = tput "rmso"

let main() =
	let regex = Str.regexp ("\\(" ^ (Array.get Sys.argv 1) ^ "\\)")
	and replace_string = (smso ^ "\\1" ^ rmso) in
	Fileutils.iter_lines (fun l ->
		print_endline(Str.global_replace regex replace_string l))
		stdin
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
