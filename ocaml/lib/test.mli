(*
 * Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 794EA259-97E8-4B59-9F93-601A98F16F53
 *)

(** This is a simple unit test framework based somewhat on oUnit. *)

type test =
	  TestCase of string * (unit -> unit)
	| TestList of test list

type test_result =
	TestSuccess of string | TestFailure of string * string

(** Test wrapper that takes a boolean function and asserts true *)
val booltest : string -> (unit -> bool) -> test

(** Test that two values are equal *)
val equaltest :
  ?stringer:('a -> string) -> string -> 'a -> (unit -> 'a) -> test

(** Run the test suite *)
val run_tests : (unit -> unit) -> (unit -> unit) -> test
	-> (test_result -> unit) -> test_result list

(** Run a test suite that doesn't require setup or teardown *)
val run_simple : test -> (test_result -> unit) -> test_result list

(** True if there's a failure in this test result list. *)
val has_failure : test_result list -> bool

(** Run and exit with an appropriate exit status
 * (0 for success, 1 for failure *)
val run_and_exit : (unit -> unit) -> (unit -> unit) -> test
    -> (test_result -> unit) -> unit

(** Run and exit with an appropriate exit status
 * (0 for success, 1 for failure *)
val run_simple_and_exit : test -> (test_result -> unit) -> unit

(** Print a test result verbosely (both failure and success) *)
val print_result_verbose : test_result -> unit

(** Print a test result normally (. for success, lots of stuff for fail) *)
val print_result : test_result -> unit
