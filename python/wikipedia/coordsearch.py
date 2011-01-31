#!/usr/bin/env python
"""
Grab the coordinates from the extracted wikipedia articles.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import re
import sys
import copy
from sqlite3 import dbapi2 as sqlite

DB=sqlite.connect("wiki.db")

PAGES_Q="""select title, article_text from interesting_pages
    where latitude is null or longitude is null
"""

UPDATE_Q="""update interesting_pages set latitude = ?, longitude = ?, type = ?
    where title = ?"""

GEO_RE=re.compile("{{(Geolink[^\|]*|coord|coor [^|]+)\|([^}]*)}}", re.M)
NUM=re.compile(r'([-\d\.]+)')

SIGNS={'N': 1, 'S': -1, 'E': 1, 'W': -1}

# Convert sexagesimal to decimal
def dms(d, m=0, s=0):
    # Special case what appears to be broken decimal disguised as sexagesimal
    if m >= 60:
        rv=float(str(int(d)) + "." + str(int(m)))
    else:
        rv=d + m / 60.0 + s / 3600.0
    return rv

def valid_coordinates(v):
    return v is None or (abs(v[0]) <= 90 and abs(v[1]) <= 180)

def parse(v):
    parts=v.split('|', 9)
    rv=None
    if (u'N' in parts or u'S' in parts) and (u'E' in parts or u'W' in parts):
        tmp=copy.copy(parts)
        latstuff, lonstuff=[], []
        latsign=lonsign=0
        curr=latstuff
        done=False
        while not done:
            try:
                curr.append(float(tmp[0]))
            except ValueError:
                import traceback
                traceback.print_exc()
            del tmp[0]
            assert tmp, "Ran out of stuff %s" % str(parts)
            if tmp[0] and tmp[0] in 'NS':
                curr = lonstuff
                latsign = SIGNS[tmp[0]]
                del tmp[0]
            elif tmp[0] and tmp[0] in 'EW':
                done = True
                lonsign = SIGNS[tmp[0]]
        if latstuff and lonstuff:
            lat=dms(*latstuff) * latsign
            lon=dms(*lonstuff) * lonsign
            rv=(lat, lon)
    elif 'long=' in v and 'lat=' in v:
        d=dict([s.split('=') for s in parts])
        rv=(float(d['lat']), float(d['long']))
    else:
        def f(v):
            m=NUM.search(v)
            if m:
                return float(m.groups()[0])
            else:
                raise ValueError, "Bad value:  ``%s''" % v
        try:
            rv=(f(parts[0]), f(parts[1]))
        except ValueError:
            import traceback
            traceback.print_exc()

    if not valid_coordinates(rv):
        print "!!! Invalid coordinates %s from %s" % (str(rv), parts)
        rv=None

    return rv

def process(title, text, cur):
    v=GEO_RE.search(text)
    if v:
        groups=v.groups()
        pv=parse(groups[1])
        if pv:
            # print "Updating", title
            cur.execute(UPDATE_Q, (pv[0], pv[1], groups[0], title))
        else:
            print "Failed to parse", groups[1]
    else:
        print "NO MATCH:\n%s\n" % text
        raise str(title)

if __name__ == '__main__':

    update_cur=DB.cursor()
    select_cur=DB.cursor()

    # Load seen titles
    select_cur.execute(PAGES_Q)
    for r in select_cur:
        try:
            process(r[0], r[1], update_cur)
        except:
            # Process what we can.
            pass

    print "Committing..."
    DB.commit()
    update_cur.close()
    select_cur.close()
    DB.close()
