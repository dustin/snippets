(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: B612DCD7-2C75-11D8-A95E-000393CB0F1E
 *)

(**
 Iterate a list supplying the iteration index to each iteration.

 @param f the function to call with the iteration and element
 @param lin the list to iterate
 *)
let iteri f lin =
	let rec loop x l =
		match l with
		  [] -> ()
		| _ -> f x (List.hd l); loop (x + 1) (List.tl l)
	in loop 0 lin
;;
