#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 9-737-11-510-0030187026

import os
import sys
import sets
import shutil
import dircache

import comps
import kickstart
import analyzerpms

if __name__ == '__main__':
    components=comps.parseComps(sys.argv[1])
    packages, individuals=kickstart.getPackages(sys.argv[2])
    rpmsrc=sys.argv[3]
    files=dircache.listdir(rpmsrc)
    dest=sys.argv[4]

    # Add the default components
    for d in components.getDefaultGroups():
        packages.add(d.id)

    allPkgs=sets.Set()
    # Start with all of the default or mandatory items
    for g in [components.groups[x] for x in packages]:
        for p in g.packagelist.mandatory:
            allPkgs.add(p)
        for p in g.packagelist.default:
            allPkgs.add(p)

    for i in individuals:
        if i[0]=='-':
            allPkgs.discard(i[1:])
        else:
            allPkgs.add(i)

    pkgFiles=analyzerpms.getRequiredFromDir(rpmsrc, allPkgs)

    print "Adding", len(pkgFiles), "files"
    shutil.rmtree(dest)
    os.mkdir(dest)
    for f in pkgFiles:
        print "Copying", f
        s,d=(os.path.join(rpmsrc, f), os.path.join(dest, f))
        # shutil.copyfile(s, d)
        os.link(s, d)
