#!/usr/bin/env python

import zipfile
import psycopg;
from sys import argv
import os

import TigerTypes
import Stats

def parseDatum(input):
	rv=None

	if len(input) > 0:
		t=TigerTypes.getType(input[0])
		rv=t.parse(input)

	return rv

def load(c, zipfile, fn):
	data=zipfile.read(fn).split('\r\n')
	i=0
	stats=Stats.Stats(len(data))
	stats.setStatName(fn)
	for line in data:
		i=i+1
		datum=parseDatum(line)
		if isinstance(datum, TigerTypes.ParsedField):
			stats.start()
			c.execute(datum.toSql())
			stats.click()
			stats.stop()
			# os.write(1, ".")
			if i % 80 == 0:
				print stats.getStats()
		elif datum == None:
			pass
		else:
			print "WARNING:  " + type(datum).__name__ + " isn't a ParsedField"
	print ''

def loadAll(c, zfn, zipfile):
	successful=None
	try:
		c.execute('begin transaction')
		c.execute("insert into loaded_files values('" \
			+ os.path.basename(zfn) + "')")
		for f in zipfile.namelist():
			print "Loading " + f
			load(c, zipfile, f)
			print os.times()
		c.execute("commit")
		successful=1
	finally:
		if not successful:
			print "Rolling back."
			c.execute('rollback')

dbconn=psycopg.connect('dbname=tiger host=disk port=2345 user=dustin ' \
	+ 'password=blahblah')

c=dbconn.cursor()

for f in argv[1:]:
	try:
		zf = zipfile.ZipFile(f)
		loadAll(c, f, zf)
	except psycopg.ProgrammingError, e:
		if str(e).find("load_filesbyname"):
			print "Aready did " + f
		else:
			raise e

dbconn.close()
