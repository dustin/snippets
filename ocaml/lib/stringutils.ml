(*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 8D25F171-05DC-11D8-9773-000393DC8AE4
 *)

(**
Various string utilities that seemed to be missing from ocaml.
*)

let rec is_my_letter l c =
	if l = [] then
		false
	else
		if (List.hd l) == c then
			true
		else
			is_my_letter (List.tl l) c
;;

(* Private function to skip the next n occurrences of any character in this
 * list in this string.  Return the new offset.
 *)
let rec pvt_skip_chars s l i =
	if (i >= String.length s ) then
		String.length s
	else
		if is_my_letter l (String.get s i) then
			pvt_skip_chars s l (i+1)
		else
			i
;;

(* Find the index of one of these characters, or -1 if it doesn't exist *)
let rec str_index_of_one str l i =
	if i < String.length str then
		if is_my_letter l (String.get str i) then
			i
		else
			str_index_of_one str l (i + 1)
	else
		-1
;;

(* Private recursive function for splitting a stream in a buffer *)
let rec pvt_rec_split_chars rv str l i limit =
	if (List.length rv < (limit-1)) && (i < String.length str) then
	begin
		let pos = str_index_of_one str l i in
		if pos != -1 then
			pvt_rec_split_chars
				(rv @ [ String.sub str i (pos - i)])
				str l
				(pvt_skip_chars str l pos)
				limit
		else
			rv @ [ String.sub str i ((String.length str) - i) ]
	end
	else
		if i < String.length str then
			rv @ [ String.sub str i ((String.length str) - i) ]
		else
			rv
;;

(* Private function to skip the next n occurrences of this character in
 * this string.  Return the new offset.
 *)
let rec pvt_skip_char s c i =
	if (i >= String.length s ) then
		String.length s
	else
		if ((String.get s i) == c) then
			pvt_skip_char s c (i+1)
		else
			i
;;

(* Private recursive function for splitting a stream in a buffer *)
let rec pvt_rec_split rv str c i limit =
	if (List.length rv < (limit - 1)) && (i < String.length str) then
	begin
		if String.contains_from str i c then
			let o = String.index_from str i c in
			pvt_rec_split (rv @ [ String.sub str i (o - i)])
				str c
				(pvt_skip_char str c o)
				limit;
		else
			rv @ [ String.sub str i ((String.length str) - i) ]
	end
	else
		if i < String.length str then
			rv @ [ String.sub str i ((String.length str) - i) ]
		else
			rv
;;

(**
 Split a string into a list of Strings.
 @param s the original string
 @param c the character on which to split
 @param limit the maximum number of splits to do
 @return an array of strings of the split elements
*)
let split s c limit =
	pvt_rec_split [] s c 0 limit
;;

(**
 Split a string into a list of Strings.
 @param s the original string
 @param l the list of characters on which to split
 @param limit the maximum number of splits to do
 @return an array of strings
*)
let split_chars s l limit =
	pvt_rec_split_chars [] s l 0 limit
;;

(*
 * Test:
 * split "123 456   789" ' ' 99;;
 * split "123 456   789  " ' ' 99;;
 * split "123 456   789  " ' ' 2;;
 * split_chars "123:456- -789" [':'; ' '; '-'] 99;;
 *)

(**
 Locate a string in another string.
 @param haystack the string in which we are searching
 @param needle the thing for which we are looking
 @param offset where to begin the search (0 for the beginning)
 @return the offset of the first match, or -1 if there is no match
 *)
let rec strstr haystack needle offset =
	if ((String.length needle) + offset) > (String.length haystack) then
		-1
	else
		if (String.sub haystack offset (String.length needle)) = needle then
			offset
		else
			if (String.contains_from haystack (offset+1) (String.get needle 0))
			then
				strstr haystack needle
					(String.index_from haystack (offset+1)
						(String.get needle 0))
			else
				-1
;;

(*
 Test:
 strstr "abcdef" "def" 0;;
 strstr "abcdef" "efg" 0;;
 strstr "abcdef" "abc" 0;;
 strstr "abcdef" "xyz" 0;;
 *)
