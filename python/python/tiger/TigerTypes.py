#!/usr/bin/env python

from sys import argv
import string

# Field BV Fmt Type Beg End Len Description

def stringyname(o):
	return o.getName()

class Null:
	def __str__(self):
		return "null"

class InvalidValue:
	def __init__(self, fielddef, slice, line):
		self.fielddef=fielddef
		self.slice=slice
		self.line=line

class InvalidNumericValue(InvalidValue):
	def __str__(self):
		return "Field " + self.fielddef.getName() \
			+ " is supposed to be numeric, but this doesn't look" \
			+ " like a number:  " + self.slice

class InvalidBlankValue(InvalidValue):
	def __str__(self):
		return "Field " + self.fielddef.getName() \
			+ " (" + str(self.fielddef.beg-1) \
			+ "-" + str(self.fielddef.end) + ":  ``" \
			+ self.slice + "'')" \
			+ " does not allow blank values, but found one here:  " \
			+ self.line

class InvalidType(InvalidValue):
	def __init__(self, fieldtype, line):
		self.fieldtype=fieldtype
		self.line=line

	def __str__(self):
		return "Expect datum of type " + str(self.fieldtype[0]) \
			+ " but this line isn't that type:  " + self.line

class TigerField:
	def __init__(self, line):
		a=line.strip().split(' ', 7)
		self.field=a[0]
		self.bv=a[1]
		self.fmt=a[2]
		self.type=a[3]
		self.beg=int(a[4])
		self.end=int(a[5])
		self.length=int(a[6])
		self.descr=a[7]

	def isNullable(self):
		return self.bv == 'Yes'

	def isNumeric(self):
		return self.type == 'N'

	def getField(self):
		return self.field

	def getName(self):
		return self.field

	def __len__(self):
		return self.length

	def getDescription(self):
		return self.descr

	def parse(self, line):
		""" Parse a line based on this field definition. """
		rv = None
		slice=line[self.beg-1:self.end]
		if self.fmt == 'L':
			slice=slice.rstrip()
		elif self.fmt == 'R':
			slice=slice.lstrip()
		if not self.isNullable():
			if len(slice)==0:
				raise InvalidBlankValue(self, slice, line)
		if self.isNumeric():
			try:
				if len(slice)==0:
					rv=Null()
				else:
					rv=int(slice)
			except ValueError:
				raise InvalidNumericValue(self, slice, line)
		else:
			rv=slice
		return rv

	def __str__(self):
		rv="Field:  " + self.field + " " + str(self.length) \
			+ ", bytes" + " type:  " + self.type + ", format:  " + self.fmt
		if self.isNullable():
			rv+=' (allows null)'
		return rv

class ParsedField:
	def __init__(self, type, val):
		self.type=type
		self.val=val

	def __str__(self):
		rv="Field:  {"
		for i in range(len(self.type)):
			rv+=self.type[i].getName() + " = "
			if self.type[i].isNumeric():
				rv+=str(self.val[i])
			else:
				rv+="'" + self.val[i] + "'"
			if i+1<len(self.type):
				rv+=", "
		rv+="}"
		return(rv)

	def __getitem__(self, which):
		return self.val[which]

	def isNull(self, which):
		rv=None
		if self.type[which].isNullable():
			if self[which] == '':
				rv=1
		return(rv)

	def toSql(self):
		rv="insert into type_" + self.type.getType().lower() + "("
		rv+=string.join(map(stringyname, self.type.getFields()[1:]), ", ")
		rv+=") values("
		for i in range(1, len(self.type)):
			if self.isNull(i):
				rv+="null"
			else:
				if self.type[i].isNumeric():
					rv+=str(self[i])
				else:
					rv+= "'" + str(self[i]).replace("'", "''") + "'"
			if i+1<len(self.type):
				rv+=", "
		rv+=")"

		return(rv)

class TigerType:

	typecache=dict()

	def __init__(self, type, f):
		self.type=type
		self.fields=list()
		l=f.readline()
		while len(l)>0:
			field=TigerField(l)
			self.fields.append(field)
			l=f.readline()

	def getType(self):
		return self.type

	def getFields(self):
		return self.fields

	def __len__(self):
		return len(self.fields)

	def __getitem__(self, i):
		return self.fields[i]

	def parse(self, line):
		if line[0] != self.getType():
			print "Expected field type is " + self.getType()
			raise InvalidType(self, line)
		# Remove nulls
		line=line.replace("\0", '')
		rv=list()
		for i in range(len(self)):
			rv.append(self[i].parse(line))
		return ParsedField(self, rv)

	def __str__(self):
		return str(self.fields)

def getType(type):
	rv=None
	if TigerType.typecache.has_key(type):
		rv=TigerType.typecache[type]
	else:
		f=file("typedata/type" \
			+ type + ".txt")
		rv=TigerType(type, f)
		TigerType.typecache[type]=rv
	return rv

def main():
	t=getType(argv[1])

	for field in t.getFields():
		print "\t" + str(field)

if __name__ == '__main__':
	main()
