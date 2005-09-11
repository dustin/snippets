#!/usr/bin/env python
# Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
# $Id: google.py,v 1.5 2002/04/16 08:48:39 dustin Exp $

from __future__ import generators

import sys
import SOAPpy

class ResultsNotReady:
    """Exception raised when attempting to use search results
    before they're ready."""
    pass

class GoogleSearch:

    def __init__(self, key, proxy=None):
        """Get a new google search thing."""
        self.server=SOAPpy.SOAPProxy("http://api.google.com/search/beta2",
            namespace='urn:GoogleSearch', http_proxy=proxy)
        self.key=key
        self.results=None

    def doSearch(self, query, filter=1, restrict='', safeSearch=0,
        lr='', ie='', oe=''):
        """Perform a google search."""

        if filter: filter=True
        else: filter=False

        if safeSearch: safeSearch=True
        else: safeSearch=False

        self.query=query
        self.filter=filter
        self.restrict=restrict
        self.safeSearch=safeSearch
        self.lr=lr
        self.ie=ie
        self.oe=oe

        self._performQuery(0)

    def _performQuery(self, startId):

        self.results=self.server.doGoogleSearch(
            self.key, self.query, startId, 10, self.filter,
            self.restrict, self.safeSearch, self.lr, self.ie, self.oe)

    # Verify we have search results ready
    def __checkResults(self):
        if not self.results:
            raise ResultsNotReady

    def __iter__(self):
        """Iterate over the search results."""
        self.__checkResults()

        lastId=0
        count=0
        while 1:
            currentId=self['startIndex']
            startId=currentId

            currentId=startId
            for r in self['resultElements']:
                yield currentId, r
                currentId+=1
            # Gotta know when to stop
            startId=self['endIndex']
            if startId == lastId or (not startId == (lastId+10)):
                raise StopIteration
            lastId=startId

            # Safety check, make sure we don't do more than 100 calls ever
            count+=1
            if count>100:
                print "TOO MANY QUERIES!"
                raise StopIteration

            # Reperform query
            self._performQuery(startId)

    def __getitem__(self, which):
        self.__checkResults()
        return self.results[which]

if __name__ == '__main__':
    myKey='2hOO7zk9TTDrPe0fpnxR0Yv/5K66pVHX'
    query=sys.argv[1]
    g=GoogleSearch(myKey)
    g.doSearch(query)

    # Print meta information:
    extra=''
    if g['estimateIsExact']:
        extra=' (exact)'
    print "Estimated results for " + `query` + ":  " \
        + `g['estimatedTotalResultsCount']` + extra
    if len(g['searchTips'])>0:
        print "Tips:  " + g['searchTips']
    if len(g['searchComments']) > 0:
        print "Comments:  " + g['searchComments']
    print "Results:"
    for r in g:
        try:
            print "\t" + `r[0]` + ": " + r[1]['URL'] + " - " + r[1]['title']
        except UnicodeError:
            print "\t" + `r[0]` + ": " + r[1]['URL'] + ' - <UNICODE ERROR>'

