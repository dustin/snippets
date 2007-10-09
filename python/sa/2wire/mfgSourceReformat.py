#!/usr/bin/env python
"""
Reformat a field-separated-value file by rearranging the
fields from the given format to the desired format.
"""

import fileinput

__INFORMAT="SerialNumber|HPID|AuthCode|PCA|SoftwareVersion|WirelessId"
OUTFORMAT="SerialNumber|SoftwareVersion|HPID|PCA|AuthCode|WirelessId"

def generateFormatMap(inf, outf=OUTFORMAT):
    "Create a mapping between two field-separated-value formats."
    ia=inf.split('|')
    oa=outf.split('|')
    assert len(ia) == len(oa)
    return [ia.index(i) for i in oa]

if __name__ == '__main__':
    map=generateFormatMap(__INFORMAT)
    for line in fileinput.input():
        a=line.strip().split('|')
        print '|'.join([a[map[i]] for i in range(len(a))])
