#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import os
import sys
import sets
import string
import dircache
import rpm

TRACKING=None

class Package(object):
    def __init__(self, filename, h):
        self.filename=filename

        self.provides=list(h[rpm.RPMTAG_PROVIDES])
        self.providever=h[rpm.RPMTAG_PROVIDEVERSION]
        self.requires=h[rpm.RPMTAG_REQUIRENAME]
        self.requirever=h[rpm.RPMTAG_REQUIREVERSION]
        self.files=[]

        # hard-coded the rpm-lib provides
        if 'rpm-libs' in self.provides:
            self.provides.append('rpmlib(CompressedFileNames)')
            self.provides.append('rpmlib(PayloadFilesHavePrefix)')
            self.provides.append('rpmlib(PartialHardlinkSets)')
            self.provides.append('rpmlib(VersionedDependencies)')

        dirs=h[rpm.RPMTAG_DIRNAMES]
        diridx=h[rpm.RPMTAG_DIRINDEXES]
        if not isinstance(diridx, type([])):
            diridx=[diridx]
        basenames=h[rpm.RPMTAG_BASENAMES]
        if basenames is not None:
            for i in range(len(basenames)):
                f=basenames[i]
                self.files.append(os.path.join(dirs[diridx[i]], f))

    def __repr__(self):
        return "<Package provide=%s, provver=%s, requires=%s, reqver=%s>" \
            % (`self.provides`, `self.providever`, `self.requires`,
            `self.requirever`)

def getDetails(path, filename):
    fn=os.path.join(path, filename)

    f=open(fn)
    (h, something)=rpm.headerFromPackage(f.fileno())
    f.close()

    p=Package(filename, h)

    return p

def getPackages(path):
    packages=[]
    for f in dircache.listdir(path):
        if f[-3:] == "rpm":
            packages.append(getDetails(path, f))
    return packages

def getProvides(packages):
    allprovides={}
    for p in packages:
        for prov in p.provides:
            allprovides[prov]=p
        for file in p.files:
            allprovides[file]=p
    return allprovides

def __getRequired(provides, wanted, need):
    lookingat=[]
    for w in wanted:
        try:
            need[w]=provides[w].filename
            if TRACKING is not None:
                if provides[w].filename.find(TRACKING) >= 0:
                    print "Package", provides[w].filename, "provides", w
            lookingat.append(provides[w])
        except KeyError, e:
            print "Could not find package providing", w

    for p in lookingat:
        req=[r for r in p.requires if r not in need]
        for r in req:
            if TRACKING is not None:
                if r.find(TRACKING) >= 0 or p.filename.find(TRACKING) >= 0:
                    print "Package", p.filename, "requires", r
        if len(req) > 0:
            __getRequired(provides, req, need)

def getRequred(provides, wanted):
    need={}
    for w in wanted:
        if TRACKING is not None:
            if w.find(TRACKING) >= 0:
                print "Base wants", w
    __getRequired(provides, wanted, need)
    return sets.Set(need.values())

def getRequiredFromDir(path, wanted):
    return getRequred(getProvides(getPackages(path)), wanted)

if __name__ == '__main__':
    path=sys.argv[1]
    required=getRequiredFromDir(path, sys.argv[2:])
    print "Need %d packages" % len(required)
    print required
