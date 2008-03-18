#!/usr/bin/env python
"""

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import sys
import datetime
from sqlite3 import dbapi2 as sqlite

import sentences

"""
create table word_groups (
    comment_id integer,
    num_words integer,
    word_seq varchar(255),
    count integer
);
"""

DB=sqlite.connect("digg-tmp.sqlite3")
CUR=DB.cursor()

INS="""insert into word_groups (comment_id, num_words, word_seq, count)
    values (?, ?, ?, ?)"""
MARK="update comments set processed = 't' where id = ?"
FETCH="""select id, comment from comments
    where processed = 'f' and (up - down) > -3
    limit 800"""

def process(id, comment):
    # print "\tprocessing %d byte comment" % len(comment)
    for size in range(2, 8):
        groups={}
        for wg in sentences.extract_word_groups(comment, size):
            # length may not be the size in the case of short sentences
            if len(wg) == size:
                w=' '.join(wg)
                groups[w] = groups.get(w, 0) + 1
        for (w, c) in groups.iteritems():
            CUR.execute(INS, [id, size, w, c])
    sys.stdout.write(".")
    sys.stdout.flush()
    CUR.execute(MARK, [id])

if __name__ == '__main__':
    total = 0
    select_cur = DB.cursor()

    select_cur.execute(FETCH)
    rows=select_cur.fetchall()
    while rows:
        total += len(rows)
        print "Starting chunk of %d" % len(rows)
        for r in rows:
            process(r[0], str(r[1]))
        print ""
        DB.commit()
        select_cur.execute(FETCH)
        rows = select_cur.fetchall()

    print "Processed %d comments" % total
