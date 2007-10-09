#!/usr/bin/env python2

import re
import md5
import sys
import os
import bisect
import time

# Regex for matching the start of a log entry and pulling out the useful fields
tsmatch=re.compile("^(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}),\d+\s+(w?\d+-\d+)\s+"
    + ".*CRASH DUMP FOR .*\s+(\d+)}}")

class CrashEntry:
    def __init__(self, basePath, parts, line):
        self.basePath=basePath

        self.ts=parts[0]
        self.server=parts[1]
        if self.server[0] != 'w':
            self.server = 'w' + self.server
        self.sn=parts[2]

        self.lines=[l]

        self.panic=None
        self.build=None
        self.uptime=None

    def get(self, s):
        return self.__dict__[s]

    def __cmp__(self, other):
        return (cmp(self.ts, other.ts))

    def setPanic(self, to):
        if self.panic is None:
            self.panic=to.strip()
    
    def setBuild(self, to):
        if self.build is None:
            self.build=to.strip()

    def setUptime(self, to):
        if self.uptime is None:
            self.uptime=to.strip()

    def addLine(self, l):
        self.lines.append(l)

    def getPath(self):
        m=md5.new(str(self))
        d=m.hexdigest()
        path=self.basePath + "/" + d[:2] + "/" + d + ".txt"
        return path

    def writeLines(self):
        """We can only write once."""
        if self.lines is not None:
            path=self.getPath()
            d=os.path.dirname(path)
            if not os.path.exists(d):
                os.makedirs(d)
            f=open(path, "w")
            f.writelines(self.lines)
            f.close()
            self.lines = None
    
    def __repr__(self):
        return "<CrashEntry " + `self.__dict__` + ">"

    def __str__(self):
        return "\t".join([self.ts, self.server, self.sn, str(self.build),
            str(self.uptime), str(self.panic)])

    def getTableRow(self):
        return "<tr><td>" + "</td><td>".join(
            [self.ts, self.server, self.sn, str(self.build),
                str(self.uptime), '<a href="' + self.getPath() + '">' \
                + str(self.panic) + '</a>']) \
                + "</td></tr>"

def sortFunction(attr):
    return(lambda a,b: cmp(a.get(attr), b.get(attr)))

# Sort functions
sorts=[ 'server', 'ts', 'sn', 'build', 'uptime', 'panic' ]

# Make the html
def makeHtml(filename, entries, thissort):
    f=open(filename, "w")
    refs={}
    for s in sorts:
        if s == thissort:
            refs[s]='byrev' + s
        else:
            refs[s]='by' + s
    refs['now']=time.ctime()
    f.write("""
<html>
<head>
    <title>Available Crash Dumps (last 14 days)</title>
    <link rel="stylesheet" href="crashes.css">
</head>
<body>
<h1>Available Crash Dumps (last 14 days as of %(now)s)</h1>
<a href="index.txt">(text version)</a>
<table>
<tr>
    <th><a href="%(ts)s.html">Timestamp</a></th>
    <th><a href="%(server)s.html">Server</a></th>
    <th><a href="%(sn)s.html">Serial Number</a></th>
    <th><a href="%(build)s.html">Build</a></th>
    <th><a href="%(uptime)s.html">Uptime</a></th>
    <th><a href="%(panic)s.html">Reason</a></th>
</tr>
""" % refs)
    # Print them out (they'll be in order)
    for i in entries:
        f.write(i.getTableRow())
    
    f.write(""" </table> </body> </html> """)

def parseBuild(l):
    a=l.split()
    return a[0] + "." + a[2]

currentEntry=None
basePath="logs"

entries=[]

for l in sys.stdin:
    match=tsmatch.match(l)
    if match is None:
        if currentEntry is not None:
            currentEntry.addLine(l)
            if l.startswith("panic: "):
                currentEntry.setPanic(l[7:])
            elif l.startswith("rtBSD 3.7"):
                currentEntry.setBuild(parseBuild(l[6:]))
            elif l.startswith("Uptime: "):
                currentEntry.setUptime(l[7:])
    else:
        # This represents the beginning of an entry
        if currentEntry is not None:
            currentEntry.writeLines()
            bisect.insort(entries, currentEntry)
        currentEntry=CrashEntry(basePath, match.groups(), l)

if currentEntry is not None:
    currentEntry.writeLines()
    bisect.insort(entries, currentEntry)

makeHtml("index.html", entries, 'ts')
for s in sorts:
    entries.sort(sortFunction(s))
    makeHtml("by" + s + ".html", entries, s)
    entries.reverse()
    makeHtml("byrev" + s + ".html", entries, "rev" + s)

### HTML is done, do a text version
f=open("index.txt", "w")
f.writelines(map(lambda x: str(x) + "\n", entries))
f.close()
