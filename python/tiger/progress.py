#!/usr/bin/env python

import zipfile
from sys import argv
import os
import psycopg
import string

import counter

class ProgressChecker:

    def __init__(self):
        self.dbconn=psycopg.connect( \
            'dbname=tiger host=disk port=2345 user=dustin ' \
            + 'password=blahblah')
        self.cursor=self.dbconn.cursor()

    def isFinished(self, name):
        bn=os.path.basename(name)
        self.cursor.execute('select filename from loaded_files ' \
            + 'where filename = %(fn)s', {'fn': bn})
        rows=self.cursor.fetchall()
        return len(rows)

    def __del__(self):
        print "Closing DB connection."
        self.dbconn.close()


def main():

    pc=ProgressChecker()
    c = counter.Counter()
    totalsize = 0
    bytype=dict()
    states=dict()
    processed_states=dict()

    for d in argv[1:]:
        todo = 0
        for ftmp in os.listdir(d):
            if string.find(ftmp, ".zip") >= 0:
                f= d + "/" + ftmp
                if pc.isFinished(f):
                    processed_states[os.path.basename(d)]=1
                else:
                    nr, nrbytype=c.count(f)
                    todo += nr
                    k = os.path.basename(d)
                    if states.has_key(k):
                        states[k]+=nr
                    else:
                        states[k]=nr
                    for k in nrbytype.keys():
                        if bytype.has_key(k):
                            bytype[k] += nrbytype[k]
                        else:
                            bytype[k] = nrbytype[k]
        if todo > 0:
            totalsize = totalsize + todo
            print str(todo) + ": " + d
    print "Have about " + str(totalsize) + " records to add."
    a=bytype.keys()
    a.sort()
    for k in a:
        print "Records of type " + k + ":  " + str(bytype[k])
    print "Have not added any records for the following states:"
    a=states.keys()
    a.sort()
    for k in a:
        if not processed_states.has_key(k):
            print "\t" + k + "\t" + str(states[k])

if __name__ == '__main__':
    main()
