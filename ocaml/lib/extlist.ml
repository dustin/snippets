(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

(** Extra list utilities. *)

(**
 Iterate a list supplying the iteration index to each iteration.

 @param f the function to call with the iteration and element
 @param l the list to iterate
 *)
let iteri f l =
	let rec loop x = function
		  [] -> ()
		| head::tail -> f x head; loop (x + 1) tail
	in loop 0 l

(**
 Shuffle (unsort) a list.
 *)
let shuffle l =
	List.sort (fun a b -> (Random.int 3) - 1) l

(**
 Take a list of lists of equal length and produce a list of lists of the values
 rolled up.
 
 i.e.

  [zip [[1;2;3]; [4;5;6]; [7;8;9]; [10;11;12]] ->
      [[10; 7; 4; 1]; [11; 8; 5; 2]; [12; 9; 6; 3]]]
 *)
let zip l =
    List.fold_left (fun i acc -> List.map2 (fun a b -> b::a) i acc)
		(List.map (fun i -> [i]) (List.hd l)) (List.tl l)


(**
 Get the sublist of the given list starting at position n.

 @param l the list
 @param n the position at which to begin the sublist
 *)
let rec nthtail l = function
	  0 -> l
	| n -> nthtail (List.tl l) (n - 1)
