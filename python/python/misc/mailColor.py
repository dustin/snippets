#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: mailColor.py,v 1.1 2003/07/01 05:50:30 dustin Exp $
"""

import sys

# User mapping
userMap={}
userMap['dustin']=['dustin@spy.net']
userMap['jennifer']=['leochild98@aol.com', 'leochild@ipa.net',
	'randy.garrett@gte.net', 'scullers@aristotle.net',
	'leochild98@hotmail.com']
userMap['noelani']=['noelani@spy.net']

# Person to color map
userColors={
	'jennifer': (0,0,0),
	'dustin': (164,164, 164),
	'noelani': (164,164,164)}

#
# End of configuration
#

map={}
for k in userMap.keys():
	for a in userMap[k]:
		map[a]=k

# Build the color table and map the names to the colors
tmpcolors=[]
colormap={}
colorTable="{\\colortbl;\\red255\\green255\\blue255;"
for v in userColors.values():
	colormap[v]=1
for c in colormap.keys():
	tmpcolors.append(c)
	colormap[c]="\\cf" + `len(tmpcolors)+1`
	colorTable = colorTable \
		+ "\\red" + `c[0]` \
		+ "\\green" + `c[1]` \
		+ "\\blue" + `c[2]` \
		+ ";"
colorTable= colorTable + "}"

colors={}
for k in userColors.keys():
	colors[k]=colormap[userColors[k]]

needsep=0

# RTF Header
print """{\\rtf1\\mac\\ansicpg10000\\cocoartf102"""
print """{\\fonttbl\\f0\\fswiss\\fcharset77 Helvetica-Bold;\\f1\\fswiss\\fcharset77 Helvetica;}"""
print colorTable
print """\\margl1440\\margr1440\\vieww9120\\viewh13240\\viewkind0
\\pard\\tx720\\tx1440\\tx2160\\tx2880\\tx3600\\tx4320\\tx5040\\tx5760\\tx6480\\tx7200\\tx7920\\tx8640\\ql\\qnatural"""

l=sys.stdin.readline()
inaheader=0
while l != '':
	if l.startswith('From '):
		inaheader=1
		print "\\\n\\\n\\f0\\b"
		if needsep == 1:
			print "\\pard\\qc"
			print "\\cf2 ---------------------------------------------------"
			print "\\pard\\qnatural"
			print "\\\n\\"
		needsep=1
		a=l.split()
		print colors[map[a[1].lower()]]
	if inaheader == 1 and l == '\n':
		inaheader=0
		print "\\f1\\b0"
	print l[:-1] + '\\'
	l=sys.stdin.readline()

# RTF footer
print "}"
