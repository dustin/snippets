(** Various string utilities.  *)

val str_index_of_one : string -> char list -> int -> int
val split : string -> char -> int -> string list
val split_chars : string -> char list -> int -> string list
val strstr : string -> string -> int -> int
val ends_with : string -> string -> bool
val begins_with : string -> string -> bool
val string_of_chars : char list -> string
val string_of_char : char -> string

(*
 * arch-tag: 55FCB2C6-2B50-11D8-B4D4-000393CFE6B8
 *)
