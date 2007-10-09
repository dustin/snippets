(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

(* Generate a translation alphabet for a simple cipher. *)

Random.self_init()

let letters = [
	'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
	'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'; 
]

let newletters = Extlist.shuffle letters

module CharMap = Map.Make(Char)

(* Create the map (upper and lowercase) *)
let m = List.fold_left2 (fun m a b ->
		CharMap.add (Char.uppercase a) (Char.uppercase b) (CharMap.add a b m))
	CharMap.empty letters newletters

let main() =
	Stream.iter (fun c ->
		print_char(try CharMap.find c m with Not_found -> c))
		(Stream.of_channel stdin);
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
