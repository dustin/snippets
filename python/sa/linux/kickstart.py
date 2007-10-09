#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import sys
import sets

def getPackages(path):
    packages=sets.Set()
    individuals=sets.Set()
    f=open(path)
    inPackages=False
    for line in f:
        if line[0]!='#':
            line=line.strip()
            if line.find('%packages') == 0:
                inPackages=True
            elif line.find('%') == 0:
                inPackages=False
            elif inPackages:
                if line[0]=='@':
                    packages.add(line[1:].strip())
                else:
                    individuals.add(line)
    f.close()
    return packages, individuals

if __name__ == '__main__':
    print getPackages(sys.argv[1])
