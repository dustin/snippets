#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>

This is a hacked up attempt to extract a swf from an exe file.  It doesn't work
too well on the one file I had to test against.
"""
# arch-tag: C4D48928-2713-11D9-BF48-000A957659CC

import sys
import struct

BLOCKSIZE=8192

def copyPart(srcFile, destFile, startOffset, nBytes):
    print "Getting", nBytes, "bytes from", startOffset
    srcFile.seek(startOffset)
    # Write the output
    while(nBytes > 0):
        toread=BLOCKSIZE
        if toread > nBytes:
            toread = nBytes
        buf=srcFile.read(toread)
        assert len(buf) > 0
        nBytes -= len(buf)
        destFile.write(buf)

def decode(fnsrc, fndest):
    print "Fixing", fnsrc, "->", fndest
    # open the file
    fin=file(fnsrc)
    # Go to four bytes from the end
    fin.seek(-4, 2)
    endoffile=fin.tell()
    data=fin.read(4)
    (boff, ) =struct.unpack("<l", data)

    # OK, let's seek to that part
    # fin.seek(0 - (nbytes + 8) , 2)
    fin.seek(boff)
    nbytes=endoffile - boff

    fout=file(fndest, "w")
    copyPart(fin, fout, fin.tell(), nbytes)

    fout.close()
    fin.close()

def search(fn):
    f=file(fn)
    f.seek(0, 2)
    filelen=f.tell()
    # Rewind
    f.seek(0)
    offset=0
    while offset < filelen:
        b=f.read(1)
        offset += len(b)
        mark=offset
        if b == 'F':
            b=f.read(1)
            offset += len(b)
            if b == 'W':
                b=f.read(1)
                offset += len(b)
                if b == 'S':
                    print "Found FWS marker at " + `mark`
                    b=f.read(1)
                    offset += len(b)
                    (ver, ) = struct.unpack("b", b)
                    print "Version " + `ver`
                    if ver > 0 and ver < 8:
                        b=f.read(4)
                        flen=struct.unpack("<l", b)
                        print "Data length is " + `flen`
                        newf=file("/tmp/tmpf.swf", "w")
                        copyPart(f, newf, mark-1, (filelen - (mark-1)))
                        newf.close()
                        f.seek(offset)

def fix(filename):
    if filename[-4:] != '.exe':
        raise "Not a .exe"
    newfn = filename[:-4] + ".swf"
    # decode(filename, newfn)
    search(filename)

if __name__  == '__main__':
    for f in sys.argv[1:]:
        fix(f)
