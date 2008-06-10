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
*)
exception UnexpectedResponse of string

(** Exception raised when the server has run out of memory. *)
exception OutOfMemory

(** Exception raised if you somehow find a bug in the server. *)
exception InternalError

(**
  Exception raised indicating the server is in drain mode and no longer
  accepting jobs.
*)
exception Draining

(**
  Exception raised when the client sends a command the server understands, but
  with bad arguments or something.
*)
exception BadFormat

(**
  Exception raised when the client sends a command the server does not
  understand
*)
exception UnknownCommand

(**
  Exception raised when a reserve_with_timeout times out.
*)
exception Timeout

(** Exception raised when a queue insertion failed. *)
exception Buried of int

(** Exception raised when a put was not terminated properly. *)
exception ExpectedCRLF

(** Exception raised when a job is too big to fit in the queue. *)
exception JobTooBig

(** Exception raised when a reserved job is going to expire too soon *)
exception DeadlineSoon

(** Excepton raised when the client attempts to ignore its last ube *)
exception NotIgnored

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

(** Get (but do not reserve) the next ready job *)
val peek_ready : beanstalk_conn -> beanstalk_job

(** Get (but do not reserve) the next buried job *)
val peek_buried : beanstalk_conn -> beanstalk_job

(** Get (but do not reserve) the next delayed job *)
val peek_delayed : beanstalk_conn -> beanstalk_job

(** {2 Stats} *)

(** Overall server stats *)
val stats : beanstalk_conn -> (string, string) Hashtbl.t

(** Overall server stats with int64 values *)
val int_stats : beanstalk_conn -> (string, int64) Hashtbl.t

(** Stats for a particular job *)
val stats_job : beanstalk_conn -> int -> (string, string) Hashtbl.t

(** Stats for a particular job with int64 values *)
val int_stats_job : beanstalk_conn -> int -> (string, int64) Hashtbl.t

(** Stats for a tube *)
val stats_tube : beanstalk_conn -> string -> (string, string) Hashtbl.t

(** Stats for a tube with int64 values *)
val int_stats_tube : beanstalk_conn -> string -> (string, int64) Hashtbl.t

(** Convenience function for converting (string, string)
	Hashtbls to (string, int64)
*)
val intify_stats : (string, string) Hashtbl.t -> (string, int64) Hashtbl.t
