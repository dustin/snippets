#!/usr/bin/env python

import os
import csv
import sys

keepers = ['some', 'field', 'names', -1]

def load(fn):
    name = os.path.basename(os.path.splitext(fn)[0])
    rv = []
    with open(fn) as f:
        r = csv.reader(f)
        hdr = r.next()
        hx = dict((y,x) for x,y in enumerate(hdr))
        cols = [x if isinstance(x, int) else hx[x] for x in keepers]
        for l in r:
            if not l:
                continue
            rv.append([l[cols] for x in cols])

    return rv

w = csv.writer(sys.stdout)
w.writerow(keepers)

for c in [load(fn) for fn in sys.argv[1:]]:
    w.writerows(c)
