#!/usr/bin/env python
#
# Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
# $Id: threadutil.py,v 1.1 2002/03/27 04:01:41 dustin Exp $

import threading

def dumpThreads():
	print "Threads:"
	for t in threading.enumerate():
		print "  * " + str(t)


if __name__ == '__main__':
	import threadpool
	tp=threadpool.ThreadPool("Test Pool", 10)
	tp.shutdown()
	dumpThreads()
