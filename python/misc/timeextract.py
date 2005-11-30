#!/usr/bin/env python
"""
Extract timestamps from logs and compute the differences between lines.

Example input
(the first whitespace element is a space, and the second a tab):

2005-11-29 17:29:47,596 Inform
2005-11-29 17:29:47,689 Response
2005-11-29 17:29:49,088 [null]
2005-11-29 17:29:49,088 RPC Methods
2005-11-29 17:29:50,519 Response


Example output:

Inform :29:47.596 (+0.000) (@+0.000)
Response :29:47.689 (+0.093) (@+0.093)
[null] :29:49.088 (+1.399) (@+1.492)
RPC Methods :29:49.088 (+0.000) (@+1.492)
Response :29:50.519 (+1.431) (@+2.923)


Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 93C76BA1-96E6-43C7-8237-E38FEA38856D

import sys
import time
import fileinput

if __name__ == '__main__':

    INF="%Y-%m-%d %H:%M:%S"
    OUTF=":%M:%S"

    start=0
    last=0

    for line in fileinput.input():
        line=line.strip()
        ts, label=line.split("\t")
        parts=ts.split(",")
        tt=time.strptime(parts[0], INF)
        t=time.mktime(tt) + (float(parts[1])/1000)

        if start == 0:
            start = t

        diff = 0
        adiff = 0
        if last != 0:
            diff = t-last
            adiff = t - start

        print "%s %s.%s (+%.3f) (@+%.3f)" \
            % (label, time.strftime(OUTF, tt), parts[1], diff, adiff)

        last=t
