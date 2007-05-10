#!/usr/bin/env python
"""
Simple tool to count the occurrences of lines in input.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import sys
import fileinput

h={}
# Gather all of the lines along with the number of occurrences each.
for line in fileinput.input():
    h[line]=h.get(line, 0) + 1

# Print them out in numerical order, largest first
for p in sorted([(v,k) for k,v in h.iteritems()], reverse=True):
    sys.stdout.write("%d\t%s" % p)
