#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: skeleton.py,v 1.2 2003/01/03 08:43:26 dustin Exp $

Diff properties files for internationalization check.
"""

import java
import sys

class PropFile(java.util.Properties):

	def __init__(self, name):
		self.name=name

def loadProps(propFile):
	"""Load the properties for the given PropFile."""
	fis=java.io.FileInputStream(propFile.name)
	propFile.load(fis)
	fis.close()

def diffProps(props1, props2):
	for k in props1.keySet():
		if not props2.containsKey(k):
			print props2.name + " is missing " + k
	for k in props2.keySet():
		if not props1.containsKey(k):
			print props2.name + " contains " + k + ", but " + props1.name \
				+ " does not."

master = PropFile(sys.argv[1])
loadProps(master)
for i in sys.argv[2:]:
	pf=PropFile(i)
	loadProps(pf)

	diffProps(master, pf)
