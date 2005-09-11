#!/usr/bin/env python
"""
md5 a file into a new md5 file. (i.e. blah.file > blah.file.md5)

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
arch-tag: F0CDF6D7-D9BB-43A9-966E-636B04347BD3
"""

import sys
import md5

for fn in sys.argv[1:]:
	print "Doing", fn

	f=open(fn)
	b=f.read()
	f.close()

	m=md5.md5(b)

	fout=open(fn + ".md5", "w")
	fout.write(m.hexdigest() + "\n")
	fout.close()
