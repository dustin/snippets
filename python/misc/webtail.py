#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import sys

def tail(f, n=100, est_line_len=175):
    while True:
        try:
            f.seek(-1 * est_line_len * n, 2)
        except IOError:
            f.seek(0)

        atstart = (f.tell() == 0)

        lines = f.read().split("\n")
        if atstart or len(lines) > (n+1):
            break

        est_line_len *= 1.3

    if len(lines) > n:
        start = len(lines) - n - 1
    else:
        start = 0

    return lines[start:len(lines)-1]


if __name__ == '__main__':
    f=open(sys.argv[1])
    print '\n'.join(tail(f))
    f.close()
