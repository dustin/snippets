#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: printbin.py,v 1.1 2003/02/02 23:53:35 dustin Exp $
"""

# This reads strings of binary data and produces the ascii values for them

import sys

def bintochar(b):
	if len(b) != 8:
		raise "This doesn't look binary:  " + b
	for c in b:
		if c != '0' and c != '1':
			raise "This doesn't look binary:  " + b
	iv = 0
	for c in b:
		iv = iv * 2
		iv = iv | int(c)
	rv = chr(iv)
	return rv

def fixline(l):
	return ''.join(map(bintochar, l.split(' ')[1:]))

if __name__ == '__main__':
	for l in sys.stdin.readlines():
		sys.stdout.write(fixline(l.rstrip()))
	print ""
