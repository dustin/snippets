#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
arch-tag: 8ADEBE34-0CCA-11D9-94FB-000393CFE6B8
"""

import re
import sys

logmatch=re.compile('^(\d+\.\d+\.\d+\.\d+) (\S+) (\S+) \[([^\]]*)\] '
    + '"(\w+) (\S+) (\S+)" (\d+) (\S+) "(.*?)" "(.*?)"')

columns=('ip', 'user', 'som', 'date', 'type', 'path', 'ver', 'status', \
    'size', 'refer', 'agent')
outcols=('ip', 'user', 'date', 'type', 'path', 'size', 'refer', 'agent')

class Record:
    def __init__(self, line, stuff):
        # Deconstruct the tuple
        self.line=line
        self.parts={}
        (self.parts['ip'], self.parts['user'], self.parts['som'], \
            self.parts['date'], self.parts['type'], \
            self.parts['path'], self.parts['ver'], self.parts['status'], \
            self.parts['size'], self.parts['refer'], self.parts['agent'])=stuff

    def __repr__(self):
        return "<Record " + repr(self.parts) + ">"

    def getTableRow(self):
        rv="<tr>"
        for c in outcols:
            rv = rv + "<td>" + str(self.parts[c]) + "</td>"
        rv = rv + "</tr>"
        return rv

stuff={}

def record(rec):
    global stuff

    status=rec.parts['status']
    if not stuff.has_key(status):
        stuff[status]=[]

    stuff[status].append(rec)

print """Content-type: text/html

<html>
	<head>
		<title>Last Few Log Lines</title>
		<link rel="stylesheet" href="/~dustin/logs.css">
	</head>

<body>
"""

for l in sys.stdin.readlines():
    match=logmatch.match(l)
    if match is not None:
        rec=Record(l, match.groups())
        record(rec)
    else:
        print "<!-- NO MATCH (" + l + ") -->"

for k in stuff.iterkeys():
    print '[<a href="#%s">%s</a>]' % (k, k)

for (k,v) in stuff.iteritems():
    print '<a name="%s"></a>' % (k)
    print "<h1>" + str(k) + "</h1>"
    print '<table class="logset">'
    print '<tr>'
    for c in outcols:
        print "<th>" + c + "</th>"
    print '</tr>'
    for l in v:
        print l.getTableRow()
    print '</table>'

print """</body></html>"""
