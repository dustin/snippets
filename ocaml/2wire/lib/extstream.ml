(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 77D66570-2C78-11D8-BCEB-000393CB0F1E
 *)

(** Convert a stream of something that can be converted to a stream of
  something else using function f into that stream of something else.
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
;;
