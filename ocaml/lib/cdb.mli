
(** CDB Implementation.
	http://cr.yp.to/cdb/cdb.txt
 *)

(** {1 Building a CDB } *)

type cdb_creator = {
  table_count : int array;
  mutable pointers : (int * int) list;
  out : out_channel;
}
val open_out : string -> cdb_creator
val add : cdb_creator -> string -> string -> unit
val close_cdb_out : cdb_creator -> unit

(** {1 Iterating a CDB } *)

val iter : (string -> string -> unit) -> string -> unit

(** {1 Searching } *)

type cdb_file = {
	f: in_channel;
	tables: (int * int) array;
}

val open_cdb_in : string -> cdb_file

(*
val find : cdb_file -> string -> string
*)

(*
 * arch-tag: 55F4CBF0-2B50-11D8-BEDC-000393CFE6B8
 *)
