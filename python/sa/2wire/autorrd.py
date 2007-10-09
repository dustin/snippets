#!/usr/bin/env python
"""
Autogenerate and populate RRDs based on available stats.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import os
import re
import sys
import string
import cmsstat

### Data, config, etc...

GAUGE=['DS:DATA:GAUGE:1800:U:U',
    'RRA:MIN:0.5:1:4032', 'RRA:AVERAGE:0.5:1:4032', 'RRA:MAX:0.5:1:4032',
    'RRA:MIN:0.5:12:1152', 'RRA:AVERAGE:0.5:12:1152', 'RRA:MAX:0.5:12:1152',
    'RRA:MIN:0.5:288:1825', 'RRA:AVERAGE:0.5:288:1825', 'RRA:MAX:0.5:288:1825']
COUNTER=['DS:DATA:COUNTER:1800:U:U',
    'RRA:MIN:0.5:1:4032', 'RRA:AVERAGE:0.5:1:4032', 'RRA:MAX:0.5:1:4032',
    'RRA:MIN:0.5:12:1152', 'RRA:AVERAGE:0.5:12:1152', 'RRA:MAX:0.5:12:1152',
    'RRA:MIN:0.5:288:1825', 'RRA:AVERAGE:0.5:288:1825', 'RRA:MAX:0.5:288:1825']

# Configurable behavior by parameter pattern
templatesProto=(
    ('^cache\.remove\..*', None),
    ('^cache\.store\..*', None),
    ('^db\.pipeline\.\w+\.pool\..*', GAUGE),
    ('^transdropper\.\w+\.dropRate\..*', GAUGE),
    ('.*', COUNTER)
)

# All of the templates with compiled regexes
templates=[(re.compile(x[0]), x[1]) for x in templatesProto]

# this is a support function used to build the translation table below
def buildGoodSet(goodChars=string.letters + string.digits + "=,_+.-~",
    badChar='_'):

    allChars=string.maketrans("", "")
    badchars=string.translate(allChars, allChars, goodChars)
    rv=string.maketrans(badchars, badChar * len(badchars))
    return rv

# Translation table for for stats -> filenames.  Only keep known good chars
transt=buildGoodSet()

### Core part of the thing

def getRecordTypes(hostname, values):
    stuff=[]
    for k, v in values:
        type=None
        for pattern, t in templates:
            if pattern.match(k) is not None:
                type=t
                break
        if type is not None:
            filename=os.path.join("data", hostname, k.translate(transt))
            stuff.append( (k, v, filename, type)  )

    stuff.sort()
    return stuff

def createFile(filename, recipe):
    dirn=os.path.dirname(filename)
    if not os.access(dirn, os.X_OK):
        # sys.stderr.write("Creating dir %s\n" % (dirn,))
        os.makedirs(dirn)
    # sys.stderr.write("Creating file %s\n" % (filename,))
    print ' '.join(['create', filename, '-s', '300'] + recipe)

def updateDatum(filename, value, recipe):
    if not os.access(filename, os.F_OK):
        createFile(filename, recipe)
    print "update %s N:%d" % (filename, value)

if __name__ == '__main__':
    sf=cmsstat.StatFetcher(sys.argv[2])
    stuff=getRecordTypes(sys.argv[1], sf.getValues().items())
    for k, v, filename, type in stuff:
        updateDatum(filename, v, type)
