#!/usr/bin/env python

from __future__ import generators

import anydbm
import time
import os

import pidlock

def walkDbm(dbm):
    finished=None
    k,v=dbm.first()
    while 1:
        yield k,v
        try:
            k,v=dbm.next()
        except KeyError:
            raise StopIteration

# Prevent a collision
lock=pidlock.PidLock("nntpsucka.pid")

olddb=anydbm.open("newsdb")
newdb=anydbm.open("newsdb.new", "c")

kept=0
pruned=0
n=time.time()
for k,v in walkDbm(olddb):
    try:
        if k[0]=='a':
            age=n-float(v)
            # print "Age of " + k + ":  " + str(age)
            # Only remember stuff less than 14 days
            if age < (14*86400):
                newdb[k]=v
                kept+=1
            else:
                # print "Age of " + k + ":  " + str(age) + ", removing."
                pruned+=1
        else:
            # Not an article
            newdb[k]=v
            kept+=1
    except ValueError, e:
        print "ValueError:  " + str(e)
print "Kept " + `kept` + " entries, removed " + `pruned`

# Swap the DBs
os.rename('newsdb', 'newsdb.old')
os.rename('newsdb.new', 'newsdb')
