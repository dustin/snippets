#!/usr/bin/env python

import time
import sys
import string
import traceback

class LogEntry:

    timeFormat = "%Y-%m-%d %H:%M:%S"

    def __init__(self, line):
        self.line=line

        # Parse the timestamp
        tmp=line[:19]
        self.__timestamp=time.mktime(time.strptime(tmp, LogEntry.timeFormat))
        tmp=line[20:23]
        self.__timestamp=self.__timestamp + (float(tmp)/1000)
        # print tmp + " -> " + `self.__timestamp`

        # OK, now verify it's the right type of data
        string.index(line, "TransactionTiming")

        parts=string.split(line)
        self.__serial=parts[6]
        self.__type=parts[7]
        self.__state=parts[8]

    def getTimestamp(self):
        """Get the time at which this log entry occurred."""
        return self.__timestamp

    def getTimestampNearest(self, accuracy):
        """Get the timestamp truncated to the given number of seconds."""
        return( int(self.__timestamp/accuracy) * accuracy)

    def getType(self):
        return self.__type

    def getSerial(self):
        return self.__serial

    def getState(self):
        return self.__state

    def __repr__(self):
        return "<LogEntry sn=" + self.__serial + ", type=" + self.__type \
            + ", state=" + self.__state + ">";

class LogTiming:

    def __init__(self, start, stop, type, serial):
        self.__start=start
        self.__stop=stop
        self.__type=type
        self.__serial=serial

    def getRuntime(self):
        return(self.__stop-self.__start)

    def getType(self):
        return(self.__type)

    def getSerial(self):
        return(self.__serial)

    def getTimestampNearest(self, accuracy):
        """Get the timestamp truncated to the given number of seconds."""
        return( int(self.__start/accuracy) * accuracy)

    def __repr__(self):
        return "<LogTiming sn=" + self.__serial + ", type=" + self.__type \
            + ", start=" + `self.__start` \
            + ", runtime=" + `self.getRuntime()` + ">"

class PerBlock:

    def __init__(self):
        self.__count={}
        self.__times={}
        self.__totalObs=0

        self.__reset()

    def __reset(self):
        for k in Printer.LogTypes:
            self.__times[k]=0
            self.__count[k]=0
            self.__totalObs=0
            for t in ('start', 'end'):
                self.__count[k + t]=0

    def addObject(self, ob):
        if isinstance(ob, LogTiming):
            k=ob.getType()
            self.__times[k]=self.__times[k] + ob.getRuntime()
            self.__count[k]=1 + self.__count[k]
        elif isinstance(ob, LogEntry):
            k=ob.getType() + ob.getState()
            self.__count[k]=1 + self.__count[k]
        else:
            raise "Invalid log type:  " + `ob.__class__`

    def printEntries(self, rrd, ts):
        a=[]
        keys=[]
        for k in Printer.LogTypes:
            for t in ('time', 'count', 'start', 'end'):
                keys.append(k + t)
        # Start building the update string
        s='update ' + rrd + ' -t ' \
            + string.join(keys, ':') + ' ' + str(ts) + ':'
        for k in Printer.LogTypes:
            a.append(str(self.__times[k]))
            a.append(str(self.__count[k]))
            for t in ('start', 'end'):
                a.append(str(self.__count[k + t]))
        s=s + string.join(a, ':')
        print s


class Printer:

    LogTypes=('HB', 'BOOT', 'KICK', 'XMLRPC')

    def __init__(self, rrd, rate=60):
        self.__rrd=rrd
        self.__pb={}
        self.__rate=rate

    def printEntries(self):
        l=self.__pb.keys()
        l.sort()
        for k in l:
            pb=self.__pb[k]
            pb.printEntries(self.__rrd, k)

    def anObject(self, ob):
        t=ob.getTimestampNearest(self.__rate)

        pb=None
        if self.__pb.has_key(t):
            pb=self.__pb[t]
        else:
            pb=PerBlock()
            self.__pb[t]=pb

        pb.addObject(ob)

class Collector:

    def __init__(self):
        self.__storage={}
        for t in Printer.LogTypes:
            self.__storage[t]={}
        self.__output=[]
        self.__starts=0
        self.__ends=0

    def warn(self, ob):
        if ob.getState() == 'start':
            sys.stderr.write("Warning:  duplicate start:  "
                + ob.getSerial() + ' - ' + ob.getType() + "\n")
        else:
            sys.stderr.write("Warning:  no start for end:  "
                + ob.getSerial() + ' - ' + ob.getType() + "\n")

    def addEntry(self, ob):
        # Get the storage for this particular type of record
        h=self.__storage[ob.getType()]
        if ob.getState() == 'start':
            # If it's a start record, first make sure we don't have an
            # existing start record.  I guess we should just warn here.
            if h.has_key(ob.getSerial()):
                self.warn(ob)
            h[ob.getSerial()]=ob
            self.__starts = self.__starts + 1
        elif ob.getState() == 'end':
            if h.has_key(ob.getSerial()):
                old=h[ob.getSerial()]
                lt=LogTiming(old.getTimestamp(), ob.getTimestamp(),
                    ob.getType(), ob.getSerial())

                # Output this one
                self.__output.append(lt)
                # print "Sending out " + `lt`

                # Delete the start record
                del h[ob.getSerial()]
            else:
                self.warn(ob)
            self.__ends = self.__ends + 1
        else:
            raise "Unknown state (should be start or end):  " + ob.getState()

    def ready(self):
        return (len(self.__output) > 0)

    def getEntry(self):
        return self.__output.pop()

    def dump(self):
        # sys.stderr.write(`self.__storage` + "\n")
        sys.stderr.write("Saw " + str(self.__starts) + " starts\n");
        sys.stderr.write("Saw " + str(self.__ends) + " ends\n");

if __name__ == '__main__':

    p=Printer(sys.argv[1], 300)
    c=Collector()

    l=sys.stdin.readline()
    while l != '':
        try:
            le=LogEntry(l)
            c.addEntry(le)
            while c.ready():
                p.anObject(c.getEntry())
            p.anObject(le)
        except:
            e=sys.exc_info()
            sys.stderr.write(l + "\n");
            traceback.print_exception(e[0], e[1], e[2])
        l=sys.stdin.readline()

    p.printEntries()

    c.dump()
