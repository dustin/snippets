(*
 * Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: 3F48C8FC-2775-11D8-A202-000393CFE6B8
 *
 * Pivot a spreadsheet around a value.
 *
 * For this exercise, the first column is the uniqueness, the second is the
 * data type name, the third is the value and the rest should be ignored.
 *)

(* First, we need a Set type for storing strings *)
module StringSet = Set.Make(String)

type iter_type = {
	sn: string;
	fields: (string,string) Hashtbl.t;
}

(* Print a line of the output csv *)
let print_line all_fields it =
	if (it.sn = "serial_number") then (
		(* Special case the first line, print all of the header fields *)
		print_string("serial_number,");
		StringSet.iter (fun l -> print_string(l ^ ",")) all_fields;
		print_newline()
	) else (
		(* For all other lines, print the values in the order specified by
		all_fields
		*)
		print_string(it.sn  ^ ",");
		StringSet.iter (fun l ->
			try
				print_string((Hashtbl.find it.fields l) ^ ",")
			with Not_found ->
				print_string(",")
			) all_fields;
		(* If all values are integer, this will add them up in another column
		print_string("," ^ string_of_int(Hashtbl.fold
			(fun k v c -> c + (int_of_string v)) it.fields 0));
		*)
		print_newline()
	)

(* Convience function for sizing a hashtbl *)
let hlen h =
	Hashtbl.fold (fun k v o -> 1 + o) h 0

let main() = 
	(* First, let's find the list of all fields *)
	let all_fields = Fileutils.fold_file_lines (fun l m->
		StringSet.add (List.nth (Extstring.split l ',' 3) 1) m)
		StringSet.empty Sys.argv.(1) in

	(* Now, flip back through the file and perform the rotation *)
	let current_record = ref {sn=""; fields=Hashtbl.create 1; } in
	Fileutils.iter_file_lines (fun l ->
		let parts = Extstring.split l ',' 3 in
		if !current_record.sn <> List.hd parts then (
			(* Only print a record if we found some values *)
			if(not (!current_record.sn = "")) then
				print_line all_fields !current_record;
			current_record := {sn=List.hd parts; fields=Hashtbl.create 1; };
		);
		Hashtbl.replace !current_record.fields
			(List.nth parts 1) (List.nth parts 2);
	)
	Sys.argv.(1)
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
