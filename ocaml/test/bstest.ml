(*
 * Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
 *)

open Beanstalk

let main () =
	Printf.printf "Testing beanstalk client.\n%!";
	let bs = Beanstalk.connect "purple" 11300 in

	Beanstalk.use bs "ocamltest";
	Printf.printf "Now watching %d%!\n" (Beanstalk.watch bs "ocamltest");
	Printf.printf "Now watching %d%!\n" (Beanstalk.ignore bs "default");

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
	Beanstalk.delete bs job3.job_id;
	Printf.printf "Deleted job %d\n%!" job3.job_id;

	try
		Pervasives.ignore(Beanstalk.reserve_with_timeout bs 0);
		Printf.printf "Crap, expected a timeout\n%!"
	with Beanstalk.Timeout -> (
		Printf.printf "Got a timeout on a reserve\n%!"
	);

	Beanstalk.shutdown bs
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end

