#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: autoreport.py,v 1.1 2003/09/26 01:13:21 dustin Exp $
"""

import sys
import java

def calculateCols(data, cols):
    for k1 in data.keySet():
        v1=data[k1]
        if isinstance(v1, java.util.Map):
            # Loop through the second level
            for k2 in data[k1].keySet():
                v2=v1[k2]
                # If this is a not a map or a collection, we need it
                if not (isinstance(v2, java.util.Map)
                        or isinstance(v2, java.util.Collection)):
                    # If we don't already have this column, add it
                    if not cols.has_key(k2):
                        cols[k2]=(k1, k2)


# This will hold a bunch of tuples of column names to column path mapping,
# where the column path is itself a tuple of map names
extracols={}

fis=java.io.FileInputStream(sys.argv[1])
gis=java.util.zip.GZIPInputStream(fis)
ois=java.io.ObjectInputStream(gis)

try:
    sn=ois.readObject()
    while sn != None:
        data=ois.readObject()

        # Find any columns this guy references
        calculateCols(data, extracols)

        sn=ois.readObject()
except java.io.EOFException:
    sys.stderr.write("Finished!\n")

# print extracols
ec=[]
for k in extracols.keys():
    ec.append( (k, extracols[k]) )
ec.sort()
print ec
