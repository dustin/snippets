#!/usr/bin/env python
"""
Grab the coordinates from the extracted wikipedia articles.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import re
import sys
import sets
from sqlite3 import dbapi2 as sqlite

DB=sqlite.connect("wiki.db")
CUR=DB.cursor()

PAGES_Q="""select title, article_text from interesting_pages
    where latitude is null or longitude is null
"""

GEO_RE=re.compile("{{(Geolink[^\|]*|coord)\|([^}]*)}}", re.M)

def process(title, text):
    v=GEO_RE.search(text)
    if v:
        print title, "->", v.groups()
    else:
        print "NO MATCH:\n%s\n" % text
        raise str(title)

if __name__ == '__main__':

    # Load seen titles
    CUR.execute(PAGES_Q)
    for r in CUR:
        process(r[0], r[1])

    DB.commit()
    CUR.close()
    DB.close()
