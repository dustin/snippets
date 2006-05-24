(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 565F9316-295B-11D8-8F80-000393CB0F1E
 *)

let test_encode (encoded, decoded) =
	Test.equaltest ~stringer:(fun s -> s) ("encode " ^ decoded) encoded
		(fun () -> Base64.encode_string decoded)

let test_decode (encoded, decoded) =
	Test.equaltest ~stringer:(fun s -> s) ("decode " ^ encoded) decoded
		(fun () -> Base64.decode_string encoded)

let main() =
	let cases = [ ("d3d3LnB5dGhvbi5vcmc=", "www.python.org");
		("YQ==", "a"); ("YWI=", "ab"); ("YWJj", "abc"); ("", "");
		("YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXpBQ"
			^ "kNERUZHSElKS0xNTk9QUVJTVFVWV1hZWjAxMjM0\r\n"
			^ "NTY3ODkhQCMwXiYqKCk7Ojw+LC4gW117fQ==",
		 "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
			^ "0123456789!@#0^&*();:<>,. []{}")
		] in
	ignore(Test.run_simple (Test.TestList
		((List.map test_encode cases) @ (List.map test_decode cases)))
		Test.print_result);
	Printf.eprintf "\n"
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin ignore(main()) end
