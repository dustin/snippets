#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""

import md5
import fileinput

for line in fileinput.input():
    l=line.strip()
    print md5.new(l).hexdigest(), l
