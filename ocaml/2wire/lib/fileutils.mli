(** Functions for processing files and their contents. *)

(** {1 Functions for processing lines of files} *)

val my_open : string -> in_channel
val input_block : in_channel -> string -> int -> int -> int
val iter_lines : (string -> 'a) -> in_channel -> unit
val fold_lines : (string -> 'a -> 'a) -> 'a -> in_channel -> 'a
val conditional_iter_lines :
  (string -> unit) -> (string -> bool) -> in_channel -> unit
val operate_on_file_in : (in_channel -> 'a) -> string -> 'a
val operate_on_file_out : (out_channel -> 'a) -> string -> 'a
val iter_file_lines : (string -> 'a) -> string -> unit
val fold_file_lines : (string -> 'a -> 'a) -> 'a -> string -> 'a

(** {1 Functions for processing directories} *)

val mkdirs : Unix.file_perm -> string -> unit
val debug_dir_print : string -> string list -> 'a -> unit
val lsdir : string -> string list
val stat_func : (string -> Unix.stats) ref
val set_stat_func : (string -> Unix.stats) -> unit
val isdir : string -> bool

val dir_iter_via :
	(string -> string list) -> string ->
		(string -> string list -> 'c -> unit) -> 'c -> unit
val walk_dir_via :
	(string -> string list) ->
		string -> (string -> string list -> 'a -> unit) -> 'a -> unit

val dir_iter : string -> (string -> string list -> 'a -> unit) -> 'a -> unit
val walk_dir : string -> (string -> string list -> 'a -> unit) -> 'a -> unit

(*
 * arch-tag: 55F6A340-2B50-11D8-B865-000393CFE6B8
 *)
