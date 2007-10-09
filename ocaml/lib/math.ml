(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

(** Some math utilities. *)

open Num

let one = num_of_int 1

(** Calculate the factorial of the given parameter. *)
let rec fact x =
	if (x <=/ one) then (
		one
	) else (
		x */ (fact (pred_num x))
	)

(** Calculation the number of combinations of n taken k at a time. *)
let combo n k =
	(fact n) // ((fact k) */ (fact (n -/ k)))

(** Int wrapper for small combinations. *)
let combo_int n k =
	int_of_num (combo (num_of_int n) (num_of_int k))
