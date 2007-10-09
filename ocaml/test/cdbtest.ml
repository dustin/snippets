(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

open Cdb

let setup() =
	let c = open_out "test.cdb" in
	add c "a" "1";
	add c "b" "2";
	add c "c" "3";
	add c "c" "4";
	add c "1,473114043983" "1,473114043983";
	add c "dustin" "We're number one!";
	close_cdb_out c

let teardown() =
	Sys.remove "test.cdb"

let getFile () =
	open_cdb_in "test.cdb"

let trycatchfinally t c f =
	let rv = (try t () with x -> c (); f (); raise x) in
	f ();
	rv

let run_test name t =
	Printf.eprintf ".";
	setup ();
	let c = getFile () in
	trycatchfinally (fun () -> ignore(t c))
		(fun () -> Printf.eprintf "\nFailure in test ``%s''\n" name)
		(fun () -> ignore(close_cdb_in c); teardown ())

(** test app to create ``test.cdb'' and put some stuff in it *)
let main() =
	run_test "a" (fun c -> assert ((find c "a") = "1"));
	run_test "b" (fun c -> assert ((find c "b") = "2"));
	run_test "c" (fun c -> assert ((find c "c") = "3"));
	run_test "sn" (fun c ->
		assert ((find c "1,473114043983") = "1,473114043983"));
	run_test "dustin" (fun c ->
		assert ((find c "dustin") = "We're number one!"));
	run_test "missing" (fun c -> assert (
		try Printf.eprintf "Found %s\n" (find c "x"); false
		with Not_found -> true
		));
	run_test "stream" (fun c ->
		(try let stream = get_matches c "c" in
			assert ("3" = (Stream.next stream));
			assert ("4" = (Stream.next stream));
			Printf.eprintf "Too many elements in stream, got %s"
				(Stream.next stream);
			false
		with Stream.Failure -> true));
	run_test "double_stream" (fun c ->
		(try let s1 = get_matches c "c" in
			let s2 = get_matches c "c" in
			assert ("3" = (Stream.next s1));
			assert ("3" = (Stream.next s2));
			assert ("4" = (Stream.next s1));
			assert ("4" = (Stream.next s2));
			Printf.eprintf "Too many elements in stream, got %s"
				(Stream.next s2);
			false
		with Stream.Failure -> true));
	(* this test should fail, but doesn't *)
	run_test "test_iteration" (fun _ ->
		let htest = Hashtbl.create 1 in
		Hashtbl.add htest "a" "1";
		Hashtbl.add htest "b" "2";
		Hashtbl.add htest "c" "3";
		Hashtbl.add htest "c" "4";
		Hashtbl.add htest "e" "6";
		Hashtbl.add htest "1,473114043983" "1,473114043983";
		Hashtbl.add htest "dustin" "We're number one!";
		let hcdb = Hashtbl.create 1 in
		iter (Hashtbl.add hcdb) "test.cdb";
		(*
		assert ((Hashtbl.length htest) = (Hashtbl.length hcdb));
		*)
		let hf = Hashtbl.fold (fun k v c -> (k, v) :: c) in
		(hf htest []) = (hf hcdb [])
	);
	(*
	iter (fun k v -> print_endline(k ^ " -> " ^ v)) "test.cdb";
	*)
	(*
	print_endline("*** Stream.iter ***");
	Stream.iter print_endline (get_matches cdf "c");
	*)
	Printf.eprintf "\n"
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
