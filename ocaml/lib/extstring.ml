(*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
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

(**
	Find the index of one of these characters, or -1 if it doesn't exist.

	@param str the string to search
	@param l the list of characters
	@param i the index at which to begin the search
 *)
let rec str_index_of_one str l i =
	if i < String.length str then
		if is_my_letter l (String.get str i) then
			i
		else
			str_index_of_one str l (i + 1)
	else
		-1

(* Private recursive function for splitting a stream in a buffer.
 skipf is the character skip function for when a match is found
 rv is the list that will contain the return value
 str is the string being split
 l is the list of things on which to split
 i is the index at which to split
 limit is the maximum number of entries in the return array
 *)
let rec pvt_rec_split_chars skipf rv str l i limit =
	if (List.length rv < (limit-1)) && (i < String.length str) then (
		let pos = str_index_of_one str l i in
		if pos != -1 then (
			pvt_rec_split_chars skipf
				(rv @ [ String.sub str i (pos - i)])
				str l
				(skipf str l pos)
				limit
		) else (
			rv @ [ String.sub str i ((String.length str) - i) ]
		)
	) else (
		if i < String.length str then (
			rv @ [ String.sub str i ((String.length str) - i) ]
		) else (
			rv
		)
	)

(* Private recursive function for splitting a stream in a buffer *)
let rec pvt_rec_split skipf rv str c i limit =
	if (List.length rv < (limit - 1)) && (i < String.length str) then (
		if String.contains_from str i c then
			let o = String.index_from str i c in
			pvt_rec_split skipf
				(rv @ [ String.sub str i (o - i)])
				str c
				(skipf str c o)
				limit;
		else
			rv @ [ String.sub str i ((String.length str) - i) ]
	) else (
		if i < String.length str then
			rv @ [ String.sub str i ((String.length str) - i) ]
		else
			rv
	)

(**
 Split a string into a list of Strings.
 @param s the original string
 @param c the character on which to split
 @param limit the maximum number of splits to do
 @return an array of strings of the split elements
*)
let split s c limit =
	let rec pvt_skip_char s c i =
		if (i >= String.length s ) then (
			String.length s
		) else (
			if ((String.get s i) == c) then (
				pvt_skip_char s c (i+1)
			) else (
				i
			)
		)
		in
	pvt_rec_split pvt_skip_char [] s c 0 limit

(**
 Split a string into a list of Strings.  The difference between this function
 and split is that this function will return empty strings for successive
 instance of split characters.

 @param s the original string
 @param c the character on which to split
 @param limit the maximum number of splits to do
 @return an array of strings of the split elements
*)
let split_all s c limit =
	pvt_rec_split (fun s c i ->
					if i >= String.length s then
						String.length s
					else
						succ i)
			[] s c 0 limit

(* skip characters in a string from the given list of characters *)
let rec pvt_skip_chars s l i =
	if (i >= String.length s ) then
		String.length s
	else
		if is_my_letter l (String.get s i) then
			pvt_skip_chars s l (i+1)
		else
			i

(**
 Split a string into a list of Strings.

 @param s the original string
 @param l the list of characters on which to split
 @param limit the maximum number of splits to do
 @return an array of strings
*)
let split_chars s l limit =
	pvt_rec_split_chars pvt_skip_chars [] s l 0 limit

(**
 Split a string into a list of Strings.  The difference between this function
 and split_chars is that this function will return empty strings for successive
 instance of split characters.

 @param s the original string
 @param l the list of characters on which to split
 @param limit the maximum number of splits to do
 @return an array of strings
*)
let split_chars_all s l limit =
	pvt_rec_split_chars (fun s c i ->
							if i >= String.length s then
								String.length s
							else
								succ i)
			[] s l 0 limit

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

(*
 Test:
 strstr "abcdef" "def" 0;;
 strstr "abcdef" "efg" 0;;
 strstr "abcdef" "abc" 0;;
 strstr "abcdef" "xyz" 0;;
 *)

(**
 Does this string end with that string?

 @param src the source string
 @param pat the comparison string
 *)
let ends_with src pat =
	if ((String.length src) >= (String.length pat)) then
		(pat =
			String.sub src
				((String.length src) - (String.length pat)) (String.length pat))
	else
		false

(*
 Test:
 ends_with "test." ".";;
 ends_with "." "test.";;
 ends_with "test." "yourmom";;
*)

(**
 Does this string begin with that string?

 @param src the source string to test
 @param pat the string to check at the beginning
 *)
let begins_with src pat =
	if ((String.length src) >= (String.length pat)) then
		(pat = String.sub src 0 (String.length pat))
	else
		false

(*
 Test:
 begins_with "test." ".";;
 begins_with "." "test.";;
 begins_with "yourmom" "your";;
*)

(**
  Create a string from the list of characters provided.

  @param l the list of characters that will become the string.
  *)
let string_of_chars l =
	let rv = Buffer.create 64 in
	List.iter (fun c -> Buffer.add_char rv c) l;
	Buffer.contents rv

(**
 Convert a character to a string.

 @param c the character
 *)
let string_of_char =
	String.make 1

(**
 Remove characters from the front of the given string.
 *)
let remove_front chars s =
	let pos = pvt_skip_chars s chars 0 in
	String.sub s pos ((String.length s) - pos)

(* The whitespace characters *)
let pvt_whitespace = [' '; Char.chr 10; Char.chr 13; Char.chr 9]

(**
 Remove whitespace from the front of a string.
 *)
let strip_front =
	remove_front pvt_whitespace

(**
 Remove characters from the end of a string.
 *)
let remove_end chars s =
	let rec pvt_skip_chars_rev s i =
		if (i < 1) then
			0
		else
			if is_my_letter chars (String.get s i) then
				pvt_skip_chars_rev s (i-1)
			else
				i
		in
	let pos = pvt_skip_chars_rev s ((String.length s) - 1) in
	if pos >= 0 then
		String.sub s 0 (pos + 1)
	else
		""

(**
 Remove whitespace from the end of a string.
 *)
let strip_end =
	remove_end pvt_whitespace

(**
 Remove whitespace from both ends of a string.
 *)
let strip s =
	strip_front (strip_end s)
