#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: srzreader.py,v 1.1 2003/09/09 20:20:27 dustin Exp $
"""

import sys
import java

fis=java.io.FileInputStream(sys.argv[1])
gis=java.util.zip.GZIPInputStream(fis)
ois=java.io.ObjectInputStream(gis)

try:
    sn=ois.readObject()
    while sn != None:
        data=ois.readObject()

        print sn, data
        sn=ois.readObject()
except java.io.EOFException:
    print "Finished!"
