(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *)

(** Extra stream utilities. *)

(**
  Convert a stream of something that can be converted to a stream of
  something else using function f into that stream of something else.

  For example, if you have a Stream of strings that you want to convert to a
  stream of chars, you can use the following construct:

  {[stream_convert Stream.of_string stream_of_strings]}

  @param f the function to convert each chunk
  @param source the source stream
 *)
let stream_convert f source =
	let chunk = ref (f (Stream.next source)) in
	Stream.from (fun x ->
		try
			Stream.empty !chunk;
			try
				Stream.empty source;
				None
			with Stream.Failure ->
				chunk := (f (Stream.next source));
				Some (Stream.next !chunk)
		with Stream.Failure ->
			Some (Stream.next !chunk)
	)
