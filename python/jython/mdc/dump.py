#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: dump.py,v 1.1 2003/09/26 01:13:22 dustin Exp $
"""

import sys
import java

def report(sn, data):
    for k in data.keySet():
        p=data[k]
        if isinstance(p, java.util.Map):
            for k2 in p.keySet():
                print sn + "\t" + str(k2) + "\t" + str(p[k2])
        else:
            for i in p:
                print sn + "\tNETNODE\t" + "\t".join(map(str, i))

fis=java.io.FileInputStream(sys.argv[1])
gis=java.util.zip.GZIPInputStream(fis)
ois=java.io.ObjectInputStream(gis)

try:
    sn=ois.readObject()
    while sn != None:
        data=ois.readObject()

        report(sn, data)

        sn=ois.readObject()
except java.io.EOFException:
    sys.stderr.write("Finished!\n")
