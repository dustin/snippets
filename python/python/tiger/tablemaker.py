#!/usr/bin/env python

import TigerTypes
import string

print """
-- This is how we keep up with what we've seen.
create table loaded_files (
	filename text
);
create unique index load_filesbyname on loaded_files(filename);

"""

for type in (1, 2, 3, 4, 5, 6, 7, 8, 9, 'A', 'C', 'H', 'I', 'P', 'R', 'S','Z'):
	tablename = "type_" + str(type).lower()
	print "-- Doing " + tablename
	type=TigerTypes.getType(str(type))

	tc="create table " + tablename + " (\n"
	types=list()
	for field in type.getFields()[1:]:
		tmp="\t -- " + field.getDescription() + "\n"
		tmp+="\t" + field.getName() + " "
		if field.isNumeric():
			if len(field) < 5:
				tmp+="smallint"
			else:
				tmp+="integer"
		else:
			tmp+="varchar(" + str(len(field)) + ")"
		if not field.isNullable():
			tmp+=" not null"
		types.append(tmp)

	tc+=string.join(types, ",\n")
	tc+="\n);\n"

	print tc
