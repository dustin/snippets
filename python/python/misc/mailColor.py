#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: mailColor.py,v 1.2 2003/07/01 07:07:36 dustin Exp $
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
	'dustin': (64,64, 64),
	'noelani': (64,64,64)}

userFonts={
	'jennifer': 'plain',
	'dustin': 'italic',
	'noelani': 'italic'
}

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
header={}
msg={}
for k in userColors.keys():
	colors[k]=colormap[userColors[k]]
for k in userFonts.keys():
	if userFonts[k]=='italic':
		header[k]=('\\f2\\i\\b', '\\f3\\b0')
		msg[k]=('', '\\f1\\i0')
	else:
		header[k]=('\\f0\\b', '\\f1\\b0')
		msg[k]=('', '\\plain\\b0')

needsep=0

# RTF Header
print """{\\rtf1\\mac\\ansicpg10000\\cocoartf102"""
print """{\\fonttbl\\f0\\fswiss\\fcharset77 Helvetica-Bold;\\f1\\fswiss\\fcharset77 Helvetica;\\f2\\fswiss\\fcharset77 Helvetica-BoldOblique;\\f3\\fswiss\\fcharset77 Helvetica-Oblique;}"""
print colorTable
print """\\margl1440\\margr1440\\vieww9120\\viewh13240\\viewkind0
\\pard\\tx720\\tx1440\\tx2160\\tx2880\\tx3600\\tx4320\\tx5040\\tx5760\\tx6480\\tx7200\\tx7920\\tx8640\\ql\\qnatural"""

l=sys.stdin.readline()
user=''
inaheader=0
while l != '':
	if l.startswith('From '):
		# If we're processing a user, end the user thingy
		if user != '':
			print msg[user][1]

		# Mark that we're in a header
		inaheader=1

		# If we need a separator,  make one
		if needsep == 1:
			print "\\\n\\\n\\f0\\b"
			print "\\pard\\qc"
			print "\\cf0 ---------------------------------------------------"
			print "\\pard\\ql\\qnatural"
			print "\\\n\\"
			print "\\f1\\b0"
		needsep=1

		# Figure out the user
		a=l.split()
		user=map[a[1].lower()]

		# Set up the color
		print colors[user]
		# Start header
		print header[user][0]

	if inaheader == 1 and l == '\n':
		inaheader=0
		# End header for this user, begin body
		print header[user][1]
		print msg[user][0]

	# Print the current line, with a forced line break
	print l[:-1] + '\\'
	l=sys.stdin.readline()

# RTF footer
print "}"
