
(** Base64 Stream Encoder/Decoder *)

(** {1 Functions for encoding} *)

exception Invalid_encode_chunk of int
val char_map : char array
val encode_chunk : char list -> string
val encode_stream_chunk : char Stream.t -> int -> string option
val encode : char Stream.t -> string Stream.t
val encode_to_string : char Stream.t -> string
val encode_string : string -> string

(** {1 Functions for decoding} *)

exception Invalid_decode_chunk of int
val char_index : int array
val is_base64_char : char -> bool
val decode_chunk : char list -> string
val decode : char Stream.t -> string Stream.t
val decode_to_string : char Stream.t -> string
val decode_string : string -> string

(*
 * arch-tag: 55F2EB4A-2B50-11D8-AA6C-000393CFE6B8
 *)
