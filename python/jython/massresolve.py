#!/usr/bin/env jython

import sys
import time

import java
import net
import ResolvingRunnable

def getOrganization(hn):
	parts=hn.split(".")

	org=hn
	if parts[-1] == "com":
		org=parts[-2] + "." + parts[-1]
	elif parts[-1] == "net":
		org=parts[-2] + "." + parts[-1]

	return org


# Get a thread pool
threadpool=net.spy.util.ThreadPool("Resolver", 100)
# Get a hashtable to hold the results
results=java.util.Hashtable()

l=sys.stdin.readline()
while l != '':
	l=l.rstrip()
	a=l.split("\t")
	threadpool.addTask(ResolvingRunnable(results, a[0]))

	l=sys.stdin.readline()


threadpool.waitForTaskCount(0)
print "All tasks accepted, shutting down the pool."
threadpool.shutdown()
threadpool.waitForThreads()

print "All tasks complete."

for k in results.keys():
	print k + "\t" + getOrganization(results[k]) + "\t" + results[k]
