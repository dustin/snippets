#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
"""

import java
import com
import string

cbais=java.io.ByteArrayInputStream
cois=java.io.ObjectInputStream

cdbpath="/tw/var/dcache/manufacturing.cdb"
joinchar='|'

def printOut(a):
    # Convert everything to a string
    b=map(str, a)
    print string.join(b, joinchar)

enumerator=com.strangegizmo.cdb.Cdb.elements(cdbpath)
while enumerator.hasMoreElements():
    e=enumerator.nextElement()

    ois=None
    try:
        ois=cois(cbais(e.getData()))
        mfg=ois.readObject()
    finally:
        if ois is not None:
            ois.close()

    # The double parens may look funny, but we're passing a tuple.
    printOut((mfg.getBoxNum(), mfg.getSerialNumber(), mfg.getPca()))
