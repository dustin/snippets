(*
 * Copyright (c) 2008  Dustin Sallings <dustin@spy.net>
 *)

(**
  A beanstalk client for ocaml.

  {{:http://xph.us/software/beanstalkd/} http://xph.us/software/beanstalkd/}
*)

(** {1 Data Types} *)

(** {2 Exceptions} *)

(**
  Exception raised for any unexpected response.

  This is also raised for several that are expected, but undesired.
*)
exception UnexpectedResponse of string

(**
  Exception raised when a reserve_with_timeout times out.
*)
exception Timeout

(** {2 Types} *)

(** A connection to beanstalkd. *)
type beanstalk_conn = {
    reader : in_channel;
	writer : out_channel;
}

(** An indiviual job returned from beanstalkd. *)
type beanstalk_job = {
	job_id : int;
	job_data : string;
}

(** {1 Functions} *)

(** {2 Connection Management} *)

(**
 Connect to a beanstalk server.

 @param hostname the server's hostname
 @param port the port number on the server
*)
val connect : string -> int -> beanstalk_conn

(** Shut down a beanstalk connection *)
val shutdown : beanstalk_conn -> unit

(** {2 Tube Management} *)

(** Select the tube for new jobs *)
val use : beanstalk_conn -> string -> unit

(** Start watching a tube. *)
val watch : beanstalk_conn -> string -> int

(** Stop watching a tube. *)
val ignore : beanstalk_conn -> string -> int

(** List all known tubes *)
val list_tubes : beanstalk_conn -> string list

(** List all of the tubes you're currently watching. *)
val list_tubes_watched : beanstalk_conn -> string list

(** Get the name of the used tube (where puts go) *)
val used_tube : beanstalk_conn -> string

(** {2 Job Management} *)

(**
  Insert a new job

  @param pri the new priority of the job
  @param delay how long before the job is eligible for execution again
  @param ttr how long the job should be allowed to run once reserved
  @param data the actual job data
*)
val put : beanstalk_conn -> int -> int -> int -> string -> int

(**
  Reserve a job.
*)
val reserve : beanstalk_conn -> beanstalk_job

(**
  Reserve a job (with timeout)
*)
val reserve_with_timeout : beanstalk_conn -> int -> beanstalk_job

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
  Kick jobs out of the buried state.  Returns the number of jobs kicked.

  @param bound the maximum number of jobs to kick
*)
val kick : beanstalk_conn -> int -> int

(**
  Grab a job by ID (does not reserve).
*)
val peek : beanstalk_conn -> int -> beanstalk_job
