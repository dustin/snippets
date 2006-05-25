(*
 * Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 42FBC786-EAA9-4021-92B1-A724766B8C5F
 *)

let test_iteri name =
	Extlist.iteri (fun i el ->
		if i <> el then failwith (Printf.sprintf "Failed at %d" i))
		[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

let test_zip name =
	Test.booltest name (fun _ ->
		Extlist.zip [[1;2;3]; [4;5;6]; [7;8;9]; [10;11;12]]
			= [[10; 7; 4; 1]; [11; 8; 5; 2]; [12; 9; 6; 3]])

let test_zip_neg name =
	Test.booltest name (fun _ ->
		Extlist.zip [[1;2;3]; [4;5;6]; [7;8;9]; [10;11;12]]
			<> [[9; 7; 4; 1]; [11; 8; 5; 2]; [12; 9; 6; 3]])

let test_nthtail name =
	Test.booltest name (fun _ ->
		Extlist.nthtail [1;2;3;4;5;6;7;8;9;10;11;12] 4
		= [5;6;7;8;9;10;11;12])

let test_shuffle name =
	Test.booltest name (fun _ ->
		Extlist.shuffle [1;2;3;4;5;6;7;8;9;10;11;12]
		<> [1;2;3;4;5;6;7;8;9;10;11;12])

let main() =
	Test.run_simple_and_exit (Test.TestList [
		Test.TestCase ("test_iteri", test_iteri);
		test_zip "test_zip";
		test_zip_neg "test_zip_neg";
		test_nthtail "test_nthtail";
		test_shuffle "test_shuffle";
		]) Test.print_result
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
