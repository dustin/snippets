#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: D703AEB0-588C-428B-84DD-C973FCD5560D

import sets

LEFT=sets.Set("qwertasdfgzxcvb")
RIGHT=sets.Set("yuiophjklnm")

f=open("/usr/share/dict/words")
for w in f:
    type="b"
    w=w.strip().lower()
    ws=sets.Set(w)
    if ws.issubset(LEFT):
        type="l"
    elif ws.issubset(RIGHT):
        type="r"
    print type, w
f.close()
