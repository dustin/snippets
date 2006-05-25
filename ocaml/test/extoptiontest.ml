(*
 * Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: D48B519C-3BE0-4CB6-B74A-288F991E5385
 *)

let test_is_none1 () =
	Extoption.is_none None

let test_is_none2 () =
	not (Extoption.is_none (Some "x"))

let test_is_some1 () =
	Extoption.is_some (Some "x")

let test_is_some2 () =
	not (Extoption.is_some None)

let test_get_option () =
	"x" = Extoption.get_option (Some "x")

let test_get_option_neg () =
	try
		Extoption.get_option None;
		failwith "Expected to fail on get_option None"
	with Extoption.Empty_option ->
		()

let main() =
	Test.run_simple_and_exit (Test.TestList [
		Test.booltest "test_is_none1" test_is_none1;
		Test.booltest "test_is_none2" test_is_none2;
		Test.booltest "test_is_some1" test_is_some1;
		Test.booltest "test_is_some2" test_is_some2;
		Test.booltest "test_get_option" test_get_option;
		Test.TestCase ("test_get_option_neg", test_get_option_neg);
		]) Test.print_result
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
