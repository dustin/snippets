(*
 * Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 18FC57FE-9982-4A13-883E-FC5C15C11523
 *)

type test =
	  TestCase of string * (unit -> unit)
	| TestList of test list

type test_result =
	TestSuccess of string | TestFailure of string * string

let booltest n f = TestCase (n, (fun () -> if (not (f ())) then failwith n))

let equaltest ?stringer n e f =
	let get_msg got =
		match stringer with
			  None -> "doesn't equal"
			| Some x -> Printf.sprintf "Expected %s, got %s" (x e) (x got) in
	TestCase (n, (fun () ->
		let got = f () in
		if (not (e = got)) then
			failwith (get_msg got)))

let run_test name setup teardown f =
	setup ();
	try
		f ();
		teardown ();
		TestSuccess name
	with e ->
		teardown ();
		TestFailure (name, (Printexc.to_string e))

let run_tests setup teardown tests printfunc =
	let run name f =
		let r = (run_test name setup teardown f) in
			printfunc r; r in
	let rec loop l rv =
		match l with
			  [] -> rv
			| t :: [] -> (
			  	match t with
					  TestCase (name, f) -> (run name f) :: rv
					| TestList (l2) -> (loop l2 rv))
			| t :: tl -> (
				match t with
					  TestCase (name, f) -> (run name f) :: (loop tl rv)
					| TestList (l2) -> (loop tl rv) @ (loop l2 rv))
		in
	match tests with
		  TestCase (name, f) -> [run name f]
		| TestList (l) -> (loop l [])

let run_simple = run_tests (fun () -> ()) (fun () -> ())

let rec has_failure = function
	  [] -> false
	| TestSuccess(_)::tl -> has_failure tl
	| TestFailure (_,_)::tl -> true

let run_and_exit setup teardown tests printfunc =
	let results = run_tests setup teardown tests printfunc in
	prerr_newline ();
	exit (if has_failure results then 1 else 0)

let run_simple_and_exit tests printfunc =
	let results = run_simple tests printfunc in
	prerr_newline ();
	exit (if has_failure results then 1 else 0)

let print_result_verbose result =
	match result with
		  TestSuccess (name) ->
		  	Printf.eprintf "success: %s\n" name
		| TestFailure (name, reason) ->
			Printf.eprintf "\nFAIL:  %s - %s\n" name reason

let print_result result =
	match result with
		  TestSuccess (name) -> Printf.eprintf "."
		| TestFailure (name, reason) ->
			Printf.eprintf "\nFAIL:  %s - %s\n" name reason
