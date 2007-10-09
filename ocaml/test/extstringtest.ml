(*
 * Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
 *)

let test_endswith () =
	Extstring.ends_with "www.spy.net" ".net"

let test_endswith_neg () =
	not (Extstring.ends_with "www.spy.net" ".com")

let test_beginswith () =
	Extstring.begins_with "www.spy.net" "www."

let test_beginswith_neg () =
	not (Extstring.begins_with "www.spy.net" "ftp.")

let listtest =
	Test.equaltest ~stringer:(fun l -> "[" ^ (String.concat "; " l) ^ "]")

let strtest = Test.equaltest ~stringer:(fun s -> s)

let inttest = Test.equaltest ~stringer:string_of_int

let main() =
	Test.run_simple_and_exit (Test.TestList [
		Test.booltest "test_endswith" test_endswith;
		Test.booltest "test_endswith_neg" test_endswith_neg;
		Test.booltest "test_beginswith" test_beginswith;
		Test.booltest "test_beginswith_neg" test_beginswith_neg;
		listtest "test_split1" ["123"; "456"; "789"]
			(fun _ -> Extstring.split "123 456   789" ' ' 99);
		listtest "test_split2" ["123"; "456"; "789"]
			(fun _ -> Extstring.split "123 456   789   " ' ' 99);
		listtest "test_split3" ["123"; "456   789"]
			(fun _ -> Extstring.split "123 456   789" ' ' 2);
		listtest "test_split_chars" ["123"; "456"; "789"]
			(fun _ -> Extstring.split_chars "123:456- -789" [':'; ' '; '-'] 99);
		listtest "test_split_chars" ["123"; "456- -789"]
			(fun _ -> Extstring.split_chars "123:456- -789" [':'; ' '; '-'] 2);
		listtest "test_split_all" ["123"; "456"; ""; "789"]
			(fun _ -> Extstring.split_all "123 456  789" ' ' 99);
		listtest "test_split_chars_all" ["123"; "456"; ""; ""; "789"]
			(fun _ -> Extstring.split_chars_all
				"123:456 - 789" [' '; ':'; '-'] 99);
		inttest "test_str_index_of_one_1" 2
			(fun _ -> Extstring.str_index_of_one "test string" ['s'] 0);
		inttest "test_str_index_of_one_2" 0
			(fun _ -> Extstring.str_index_of_one "test string" ['t'] 0);
		inttest "test_str_index_of_one_3" 3
			(fun _ -> Extstring.str_index_of_one "test string" ['t'] 1);
		inttest "test_strstr-1" 0
			(fun _ -> Extstring.strstr "test string" "test" 0);
		inttest "test_strstr-2" 2
			(fun _ -> Extstring.strstr "test string" "st s" 0);
		inttest "test_strstr-2" 12
			(fun _ -> Extstring.strstr "test string test" "test" 1);
		strtest "test_string_of_chars" "hello"
			(fun _ -> Extstring.string_of_chars ['h'; 'e'; 'l'; 'l'; 'o']);
		strtest "test_string_of_char" "h"
			(fun _ -> Extstring.string_of_char 'h');
		strtest "test_remove_front" "spy.net"
			(fun _ -> Extstring.remove_front ['w'; '.'] "www.spy.net");
		strtest "test_strip_front" "spy.net"
			(fun _ -> Extstring.strip_front "       spy.net");
		strtest "test_remove_end" "spy"
			(fun _ -> Extstring.remove_end ['.'; 'n'; 'e'; 't'] "spy.net");
		strtest "test_strip_end" "spy.net"
			(fun _ -> Extstring.strip_end "spy.net    ");
		strtest "test_strip" "spy.net"
			(fun _ -> Extstring.strip "     spy.net    ");
		]) Test.print_result
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
