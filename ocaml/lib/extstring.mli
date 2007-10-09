(** Various string utilities.  *)

val str_index_of_one : string -> char list -> int -> int
val split : string -> char -> int -> string list
val split_chars : string -> char list -> int -> string list
val split_all : string -> char -> int -> string list
val split_chars_all : string -> char list -> int -> string list
val strstr : string -> string -> int -> int
val ends_with : string -> string -> bool
val begins_with : string -> string -> bool
val string_of_chars : char list -> string
val string_of_char : char -> string

val remove_front : char list -> string -> string
val strip_front : string -> string
val remove_end : char list -> string -> string
val strip_end : string -> string

val strip : string -> string
