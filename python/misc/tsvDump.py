#!/usr/bin/env python
"""
Dump a tab delimited file in such a way that makes it easier to read.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 608E2BE6-66D9-4EAE-AD22-E0C45DD88695

import fileinput
for line in fileinput.input():
    a=line.strip().split("\t")
    for i in range(len(a)):
        print "a[" + `i` + "] = " + a[i]
    print "---"
