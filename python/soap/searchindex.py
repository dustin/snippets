#!/usr/bin/env python
"""

Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
$Id: searchindex.py,v 1.3 2002/04/26 09:03:39 dustin Exp $
"""

import anydbm
import time
import sys
import google

class MatchPrinter:

    def __init__(self, query):
        self.seenUrl=None
        self.query=query

    def match(self, type, value):
        if type=='url':
            if not self.seenUrl:
                print "New URLs for ``" + self.query + "'':"
                self.seenUrl=1
            print "\t+ " + value['URL']
        else:
            print 'New ' + type + ' for ``' + self.query + "'':  " + value

class SearchWatcher:
    """Watch a set of search terms for result changes."""

    def __init__(self, key, proxy=None):
        self.db=anydbm.open("searchdb", "c")
        self.key=key
        self.proxy=proxy

    def __checkTips(self, g, query, matchCallback):
        tips=g['searchTips']
        try:
            oldtips=self.db[query + '/tips']
        except KeyError:
            oldtips=''
        if tips != oldtips:
            self.db[query+'/tips']=tips
            matchCallback('tips', tips)

    def __checkComments(self, g, query, matchCallback):
        comments=g['searchComments']
        try:
            oldcomments=self.db[query + '/comments']
        except KeyError:
            oldcomments=''
        if comments != oldcomments:
            self.db[query+'/comments']=comments
            matchCallback('tips', tips)

    def checkQuery(self, query, matchCallback=None):

        if matchCallback==None:
            mp=MatchPrinter(query)
            matchCallback=mp.match

        g=google.GoogleSearch(self.key, self.proxy)
        g.doSearch(query)

        # Check tips and comments
        self.__checkTips(g, query, matchCallback)
        self.__checkComments(g, query, matchCallback)

        i=0
        for rs in g:
            idx,r = rs
            key=query+'/results ' + r['URL']
            if not self.db.has_key(key):
                self.db[key]=str(time.time())
                matchCallback('url', r)
            i+=1
            # Don't do more than 256 results
            if i > 256:
                break

if __name__=='__main__':
    sw=SearchWatcher('2hOO7zk9TTDrPe0fpnxR0Yv/5K66pVHX', proxy='juan:3128')
    for q in sys.argv[1:]:
        sw.checkQuery(q)
