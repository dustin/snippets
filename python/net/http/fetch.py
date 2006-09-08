#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: A538F9C4-74FB-458C-8FFB-02768F789B50

import os
import sys
import shutil
import signal
import urllib2

# You have thirty seconds to comply
signal.alarm(30)

wantedfile=sys.argv[2]
tmpfile=wantedfile + ".tmp"

i=urllib2.urlopen(sys.argv[1])
o=open(tmpfile, "w")

shutil.copyfileobj(i, o)
os.rename(tmpfile, wantedfile)
