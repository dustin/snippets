#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: dbrecorder.py,v 1.1 2003/09/26 01:13:21 dustin Exp $

Create copy files from an srz.
"""

import sys

import com
import net
import java

class DBFileRecorder(com.twowire.app.mdcscrape.AbstractMDCRecorder):

	def __init__(self, conf, txfileName="txfile", dfileName="dfile"):
		self.conf=conf
		gl=com.twowire.app.mdcscrape.MDCLookup.getInstance()
		self.le=gl.getLookupEntries(conf, "data_list", "data_key")
		self.gpk=net.spy.db.GetPK.getInstance()
		self.txfile=java.io.FileWriter(txfileName)
		self.dfile=java.io.FileWriter(dfileName)

	def __getTransId(self):
		return self.gpk.getPrimaryKey(self.conf, "txn_list")

	def __writeOut(self, id, p):
		sn=p.getSerialNumber()
		m=p.getResults()

		ts=m['endDate']
		if ts is None:
			ts = java.util.Date()
		self.txfile.write(`id` + "\t" + sn + "\t" + `ts` + "\n")

		for v in p.getResults().values():
			if isinstance(v, java.util.Map):
				for e in v.entrySet():
					lk=e.getKey()
					lv=e.getValue()

					if self.le.containsKey(lk):
						dk=self.le[lk]
						self.dfile.write(`id` + "\t" + `dk` + "\t" + lv + "\n")
					else:
						if not lk == 'components':
							print "NO KEY FOR " + `lk` + " would have stored " \
								+ `lv`


	def reportSuccessfulJob(self, p):
		id=self.__getTransId()
		# print "DBRecorder sees " + `p` + " which will be txn_id: " + `id`
		self.__writeOut(id, p)

	def finished(self):
		print "Finished with the DB recorder"
		self.txfile.close()
		self.dfile.close()

if __name__ == '__main__':
	conf=net.spy.SpyConfig(sys.argv[1])
	print conf
	dbconf=net.spy.SpyConfig(sys.argv[2])
	print dbconf
	howMany=int(sys.argv[3])

	recorder=DBFileRecorder(dbconf)

	main=com.twowire.app.mdcscrape.MDCMain(conf)
	main.addRecorder(recorder)
	main.go(howMany, sys.stdin)

# for i in range(100):
