(*
 * Copyright (c) 2008  Dustin Sallings <dustin@spy.net>
 *)

exception UnexpectedResponse of string

exception Timeout

type beanstalk_conn = {
    reader : in_channel;
	writer : out_channel;
}

type beanstalk_job = {
	job_id : int;
	job_data : string;
}

val connect : string -> int -> beanstalk_conn
val shutdown : beanstalk_conn -> unit

val use : beanstalk_conn -> string -> unit
val watch : beanstalk_conn -> string -> int
val ignore : beanstalk_conn -> string -> int

(**
  Insert a new job

  @param priority the new priority of the job
  @param delay how long before the job is eligible for execution again
  @param ttr how long the job should be allowed to run once reserved
  @param bytes the actual job data
*)
val put : beanstalk_conn -> int -> int -> int -> string -> int

(**
  Reserve a job.
*)
val reserve : beanstalk_conn -> beanstalk_job

(**
  Delete a reserved job
*)
val delete : beanstalk_conn -> int -> unit

(**
  Release a reserved job (to be executed later).

  @param id the job ID
  @param priority the new priority of the job
  @param delay how long before the job is eligible for execution again
*)
val release : beanstalk_conn -> int -> int -> int -> unit

(**
  Bury a job.

  @param id the job ID
  @param pri the job's new priority
*)
val bury : beanstalk_conn -> int -> int -> unit

(**
  Reserve a job (with timeout)
*)
val reserve_with_timeout : beanstalk_conn -> int -> beanstalk_job

(** List all known tubes *)
val list_tubes : beanstalk_conn -> string list

(** List all of the tubes you're currently watching. *)
val list_tubes_watched : beanstalk_conn -> string list

(** Get the name of the used tube (where puts go) *)
val used_tube : beanstalk_conn -> string
