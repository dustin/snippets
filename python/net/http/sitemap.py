#!/usr/bin/env python
"""
Dig through a sitemap and grab some sample pages.

Copyright (c) 2008  Dustin Sallings <dustin@spy.net>
"""

import sys
import time
import urllib
import random

import elementtree.ElementTree as ET

SITEMAPNS="http://www.sitemaps.org/schemas/sitemap/0.9"
PREFIX={'ns': SITEMAPNS}

R=random.Random()
SAMPLE_SIZE=5

def timefetch(url, label, transform):
    f=rv=None
    start = time.time()
    status="ok"
    try:
        f=urllib.urlopen(url)
        rv = transform(f)
    except KeyboardInterrupt:
        raise
    except:
        status="err"
    finally:
        end = time.time()
        print "%s %s %s %.3f" % (label, url, status, end-start)
        sys.stdout.flush()
        if f:
            f.close
    return rv

def parse_sitemap(url):
    t = timefetch(url, "+", lambda f: ET.parse(f))
    if not t: return None
    pages=[u.text for u in t.findall(".//{%(ns)s}url/{%(ns)s}loc" % PREFIX)]
    maps=[u.text for u in t.findall(".//{%(ns)s}sitemap/{%(ns)s}loc" % PREFIX)]
    print ". found %d pages and %d maps" % (len(pages), len(maps))
    tofetch=pages
    if len(tofetch) > SAMPLE_SIZE:
        tofetch=R.sample(tofetch, SAMPLE_SIZE)
    for u in tofetch:
        timefetch(u, "- 1", lambda f: None)
        timefetch(u, "- 2", lambda f: None)
    for u in maps:
        parse_sitemap(u)

if __name__ == '__main__':
    try:
        url = parse_sitemap(sys.argv[1])
    except IndexError:
        sys.stderr.write("Hey.  I need a sitemap URL to start with.\n")
        sys.exit(64)
    parse_sitemap(url)
