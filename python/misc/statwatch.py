#!/usr/bin/env python2.3
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: statwatch.py,v 1.4 2003/07/17 16:37:44 dustin Exp $
"""

import sys
import time
import string
import urllib
import traceback

# Check for threading support and enable a multi-threaded update
try:
    import threading

    class UpdateThread(threading.Thread):
        """Thread that updates a host."""

        def __init__(self, host, prefix=""):
            """Initialize the thread"""
            # Super init
            threading.Thread.__init__(self)

            self.host=host
            self.prefix=prefix

            self.setDaemon(True)
            self.start()

        def run(self):
            """Perform the update"""
            try:
                self.host.update(self.prefix)
            except:
                self.host.setException(sys.exc_info())

    # Remember that we have threading support
    HAS_THREADING=True

except ImportError:
    # No thread support
    HAS_THREADING=False

class Host:
    """A host to watch"""

    def __init__(self, url, shortName=None):
        if shortName is None:
            self.shortName=url
        else:
            self.shortName=shortName
        self.url=url
        self.vals={}
        self.exception=None

    def update(self, prefix=""):
        """Update this host with values from the given URL"""
        u=urllib.URLopener()
        theurl=self.url + "/" + prefix
        r=u.open(theurl)
        h={}
        for l in r.readlines():
            l=l.rstrip()
            (k, v) = l.split(' = ')
            v=int(v)
            h[k] = v
        self.vals=h

    def setException(self, e):
        self.exception=e

    def failed(self):
        return self.exception is not None

    def printExc(self):
        if(self.failed()):
            print "***  Error on " + str(self) + ":"
            print "***   " + traceback.format_exception_only(\
                self.exception[0], self.exception[1])[0]

            self.exception=None

    def __repr__(self):
        return "<Host: " + self.shortName + " - " + self.url + ">"

    # Alias str() to ``
    __str__ = __repr__

def signed(x):
    rv=""
    if x > 0:
        rv = "+" + `x`
    else:
        rv=`x`
    return rv

def mergeDicts(dicts):
    """Merge a list of dictionaries into one dictionary with the sum of the
    values of all of the dictionaries."""
    rv={}
    for h in dicts:
        for k,v in h.iteritems():
            ov=rv.get(k, 0)
            rv[k]=(v + ov)
    return rv

if HAS_THREADING:
    def doUpdate(hosts, prefix=""):
        """Have all the host objects update themselves"""
        threads=[]
        for host in hosts:
            try:
                threads.append(UpdateThread(host, prefix))
            except IOError:
                print "***  Error on " + str(host) + ":"
                print "***   " + traceback.format_exception_only(\
                    sys.exc_type, sys.exc_value)[0]

        # Wait for all of the threads to finish
        for t in threads:
            t.join()
else:
    sys.stderr.write("\n!!!Warning:  no thread support!\n\n")
    def doUpdate(hosts, prefix=""):
        """Have all the host objects update themselves"""
        for host in hosts:
            try:
                host.update(prefix)
            except IOError:
                host.setException(sys.exc_info())
                # print "***  Error on " + str(host) + ":"
                # print "***   " + traceback.format_exception_only(\
                    # sys.exc_type, sys.exc_value)[0]

def updateAll(hosts, oldvals, prefix=""):
    """Update all of the hosts into the given dict.  Return the deltas"""
    doUpdate(hosts, prefix)
    dicts=[]
    for host in hosts:
        if host.failed():
            host.printExc()
        else:
            dicts.append(host.vals)
    # Merge all the dicts
    tmp=mergeDicts(dicts)

    # Now that we've got all the individuals, add them all up and calculate
    # deltas
    deltas={}
    ks=tmp.keys()
    ks.sort()
    for k in ks:
        v=tmp[k]
        ov=oldvals.get(k, 0)
        if v != ov:
            d=(v-ov)
            deltas[k]=d
            # Display the current value
            print k + ":  " + `v` + " (" + signed(d) + ")"
        oldvals[k]=v
    return deltas

def showTimes(times, readings):
    for timeset in times:
        try:
            label=timeset[0]
            timing=float(readings[timeset[1]])
            transcount=0.0
            for k in readings.keys():
                if k.startswith(timeset[2]):
                    transcount = transcount + float(readings[k])
            if transcount > 0 and timing > 0:
                rate=((timing / transcount)/1000)
                print "%s: %.2fs/t" % (label, rate)
        except KeyError:
            # No data found for this key
            pass

def getServers():
    # Build the list of servers
    servers={}

    u=urllib.URLopener()
    r=u.open("http://buildmaster.eng.2wire.com/clusterinfo/clusters.txt")
    for cluster in r.readlines():
        cluster=cluster.strip()
        tmp=[]
        rl=u.open("http://buildmaster.eng.2wire.com/clusterinfo/" + cluster \
            + ".txt")
        for s in rl.readlines():
            s=s.strip()
            parts=s.split("\t")
            url=parts[2] + "/admin/monitor/stat"
            tmp.append(Host(url, parts[0]))
        servers[cluster] = tmp

    return servers

if __name__ == '__main__':

    # Time calculation stuff
    timeCalcs=( ('Heartbeat', 'rpc.time.HB', 'rpc.success.CMS_HEARTBEAT'),
                ('Bootstrap' ,'rpc.time.BOOT', 'rpc.success.CMS_BOOTSTRAP'),
                ('Kick', 'rpc.time.KICK' ,'rpc.success.CMS_KICKED'),
            )

    h={}
    hparam=''
    if len(sys.argv) > 1:
        hparam=sys.argv[1]

    # Get the server list
    servers=getServers()
    hosts=servers.get(hparam, None)

    # If we didn't get one, print a nice happy error
    if hosts is None:
        snames=servers.keys()
        snames.sort()
        print ""
        print "The following clusters are available:"
        for s in snames:
            print "\t" + s
        print ""
        raise "Invalid cluster:  " + sys.argv[1]
    print "Using the following URLs:  " + `hosts`

    # Check to see if a prefix was applied
    prefix=""
    if len(sys.argv) > 2:
        prefix=sys.argv[2]
        print "Limited to this prefix:  " + prefix

    # Loop forever
    while 1:
        print "--------------------------------------- " \
            + time.ctime(time.time())
        deltas=updateAll(hosts, h, prefix)
        print ""
        showTimes(timeCalcs, deltas)
        print ""
        time.sleep(60)
