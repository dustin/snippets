#!/usr/bin/env python

import argparse
import csv
import os
import sys

def load(fn, keepers, prefix=[]):
    name = os.path.basename(os.path.splitext(fn)[0])
    rv = []
    with open(fn) as f:
        r = csv.reader(f)
        hdr = r.next()
        hx = dict((y,x) for x,y in enumerate(hdr))
        cols = [x if isinstance(x, int) else hx[x] for x in keepers]
        for l in r:
            if not l:
                continue
            rv.append(prefix + [l[x] for x in cols])

    return rv

def maybeint(x):
    try:
        return int(x)
    except ValueError:
        return x

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Pull common fields from CSVs.')
    parser.add_argument('--cols', metavar='col,names', default='', help='output column names')
    parser.add_argument('--keep', metavar='col,names', default='', help='source columns to retain')
    parser.add_argument('--prefix', metavar='value', default='', help='extra column for each row')
    parser.add_argument('--prefix_file', action='store_true',
                        help='prefix each row with the basename of the source file')
    parser.add_argument('files', metavar='file', type=str, nargs='+')
    args = parser.parse_args()

    keepers = [maybeint(a) for a in args.keep.split(',')]

    w = csv.writer(sys.stdout)
    w.writerow(args.cols.split(',') if args.cols else keepers)

    for fn in args.files:
        prefixes = []
        if args.prefix_file:
            prefixes.append(os.path.basename(os.path.splitext(fn)[0]))
        if args.prefix:
            prefixes.append(args.prefix)
        w.writerows(load(fn, keepers, prefixes))
