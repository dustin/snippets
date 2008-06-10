(*
 * Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
 *)

open Beanstalk

let s_hash_print = Hashtbl.iter (Printf.printf "  %s -> %s\n%!")

let l_hash_print = Hashtbl.iter (Printf.printf "  %s -> %Ld\n%!")

let main () =
	Printf.printf "Testing beanstalk client.\n%!";
	let bs = Beanstalk.connect (
		try Sys.argv.(1)
		with Invalid_argument("index out of bounds") -> "localhost"
		) 11300 in

	Beanstalk.use bs "ocamltest";
	Printf.printf "Now watching %d%!\n" (Beanstalk.watch bs "ocamltest");
	Printf.printf "Now watching %d%!\n" (Beanstalk.ignore bs "default");
	try Pervasives.ignore(Beanstalk.ignore bs "ocamltest")
	with NotIgnored -> ();

	Printf.printf "Current tubes:\n%!";
	List.iter (Printf.printf " ``%s''\n%!") (Beanstalk.list_tubes bs);

	Printf.printf "Watched tubes:\n%!";
	List.iter (Printf.printf " ``%s''\n%!") (Beanstalk.list_tubes_watched bs);

	Printf.printf "Used tube:  %s\n%!" (Beanstalk.used_tube bs);

	Printf.printf "Inserted %d\n%!" (
		Beanstalk.put bs 100 0 120 "test1 from ocaml");
	Printf.printf "Inserted %d\n%!" (
		Beanstalk.put bs 100 0 120 "test2 from ocaml");

	let job = Beanstalk.reserve bs in
	Printf.printf "Got job %d:  %s\n%!" job.job_id job.job_data;

	Beanstalk.release bs job.job_id 100 0;
	Printf.printf "Released job %d\n%!" job.job_id;

	let job2 = Beanstalk.reserve_with_timeout bs 0 in
	Printf.printf "Got job %d:  %s\n%!" job2.job_id job2.job_data;

	let job3 = Beanstalk.reserve_with_timeout bs 0 in
	Printf.printf "Got job %d:  %s\n%!" job3.job_id job3.job_data;

	Beanstalk.delete bs job2.job_id;
	Printf.printf "Deleted job %d\n%!" job2.job_id;

	Beanstalk.bury bs job3.job_id 100;
	Printf.printf "Buried %d\n%!" job3.job_id;

	let peekedb = Beanstalk.peek_buried bs in
	Printf.printf "Peeked (buried) job %d:  %s\n%!"
		peekedb.job_id peekedb.job_data;

	try
		let tmp = Beanstalk.reserve_with_timeout bs 0 in
		Printf.printf "Crap, expected a timeout, got job %d\n%!" tmp.job_id
	with Beanstalk.Timeout -> (
		Printf.printf "Got a timeout on a reserve\n%!"
	);

	let peeked = Beanstalk.peek bs job3.job_id in
	Printf.printf "Peeked job %d:  %s\n%!" peeked.job_id peeked.job_data;

	Printf.printf "Kicked %d\n%!" (Beanstalk.kick bs 100);

	let peekedr = Beanstalk.peek_ready bs in
	Printf.printf "Peeked (ready) job %d:  %s\n%!"
		peekedr.job_id peekedr.job_data;

	let job4 = Beanstalk.reserve_with_timeout bs 0 in
	Printf.printf "Got job %d:  %s\n%!" job4.job_id job4.job_data;

	Beanstalk.release bs job4.job_id 100 2;
	Printf.printf "Released job %d (with timeout)\n%!" job4.job_id;

	let peekedd = Beanstalk.peek_delayed bs in
	Printf.printf "Peeked (delayed) job %d:  %s\n%!"
		peekedd.job_id peekedd.job_data;

	Printf.printf "Job stats for %d:\n%!" peekedd.job_id;
	s_hash_print (Beanstalk.stats_job bs peekedd.job_id);

	Printf.printf "Int job stats for %d:\n%!" peekedd.job_id;
	l_hash_print (Beanstalk.int_stats_job bs peekedd.job_id);

	let job5 = Beanstalk.reserve_with_timeout bs 3 in
	Printf.printf "Got job %d:  %s\n%!" job5.job_id job5.job_data;

	Beanstalk.delete bs job5.job_id;
	Printf.printf "Deleted job %d\n%!" job5.job_id;

	try
		let peeked2 = Beanstalk.peek bs job3.job_id in
		Printf.printf "Crap, expected Not_found, got job %d\n%!" peeked2.job_id
	with Not_found -> Printf.printf "Couldn't find %d again.\n%!" job3.job_id;

	Printf.printf "Stats:\n%!";
	s_hash_print (Beanstalk.stats bs);
	Printf.printf "Int stats:\n%!";
	l_hash_print (Beanstalk.int_stats bs);

	Printf.printf "Tube stats:\n%!";
	s_hash_print (Beanstalk.stats_tube bs "ocamltest");

	Printf.printf "Tube int stats:\n%!";
	l_hash_print (Beanstalk.int_stats_tube bs "ocamltest");

	Beanstalk.shutdown bs
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end

