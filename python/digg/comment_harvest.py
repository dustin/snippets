#!/usr/bin/env python
"""
Digg comment harvester.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import sys
import time
import datetime
from sqlite3 import dbapi2 as sqlite

import digg

# <comment date="1202272575" id="12644124" story="5129500" up="1" down="0"
# replies="0" replyto="12639792" user="kretik" level="1" root="12639792">

"""
create table comments (
    id integer not null,
    ds datetime not null,
    story_id integer not null,
    user varchar(16) not null,
    up integer not null,
    down integer not null,
    replies integer not null,
    level integer not null,
    replyto integer,
    root integer,
    comment text not null
);
"""

DB=sqlite.connect("digg.sqlite3")
CUR=DB.cursor()

INS_QUERY="""insert into comments (id, ds, story_id, user, up, down,
    replies, level, replyto, root, comment)
    values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
"""

def date_seq(date):
    """Get the starting timestamp for every hour from the given start date."""
    start = int(time.mktime(date.timetuple()))
    stop = start + 86400
    return range(start, stop, 3600)

def time_ago_seq(ndays=1):
    """Get the sequence for yesterday"""
    return date_seq(datetime.date.today() - datetime.timedelta(ndays))

def store_comment(comment):
    """Store a comment."""
    print "\t\t%s: %s+/%s-" %(comment.user, comment.up, comment.down)
    replyto=getattr(comment, 'replyto', None)
    root=getattr(comment, 'root', None)
    CUR.execute(INS_QUERY, (comment.id,
        datetime.datetime.fromtimestamp(int(comment.date)),
        comment.story, comment.user, comment.up, comment.down, comment.replies,
        comment.level, replyto, root, comment.text
        ))

def do_comments(range):
    """Fetch and process comments for the given date range."""
    d=digg.Digg('http://code.google.com/p/pydigg/')
    size=100
    for min_date in range:
        print "Starting %s" % time.ctime(min_date)
        offset = total = 0
        done = False
        while not done:
            print "\tfetching (%d/%d)..." % (offset, total)
            comments = d.getComments(min_date=min_date, count=size,
                sort='', offset=offset)
            assert len(comments) == int(comments.count)
            print "\tgot %d items" % len(comments)
            offset = int(comments.offset) + size
            total = int(comments.total)
            done = int(comments.offset) + int(comments.count) >= total
            for c in comments:
                store_comment(c)
            DB.commit()

if __name__ == '__main__':
    n=0
    if len(sys.argv) > 1:
        n=int(sys.argv[1])
    do_comments(time_ago_seq(n))
