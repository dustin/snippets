#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 67C0E18C-42A1-11D9-B6FD-000A957659CC

import sys
import os

def glob2re(s):
    return s.strip().replace(".", "\\.").replace("*", ".*")

def convert(dirpath):
    fin=open(dirpath + "/.cvsignore")
    print dirpath
    p="|".join([glob2re(x) for x in fin])
    fin.close()
    fout=open(dirpath + "/.arch-inventory", "w")
    fout.write("precious ^(" + p + ")$\n")
    fout.close()

if __name__ == '__main__':
    for d in sys.argv[1:]:
        for dirpath, dirnames, filenames in os.walk(d):
            if ".cvsignore" in filenames:
                convert(dirpath)
