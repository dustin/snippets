#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 02C5C200-4940-11D9-9AD5-000393CFE6B8

import md5
import fileinput

for line in fileinput.input():
    l=line.strip()
    print md5.new(l).hexdigest(), l
