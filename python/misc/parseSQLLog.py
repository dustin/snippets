#!/usr/bin/env python

import time
import sys
import string

class LogEntry:

    timeFormat = "%Y-%m-%d %H:%M:%S"

    def __init__(self, line):
        self.line=line

        # Parse the timestamp
        tmp=line[:19]
        self.__timestamp=time.mktime(time.strptime(tmp, LogEntry.timeFormat))

        # OK, now verify it's the right type of data
        string.index(line, "database.DBManager.sql")

        parts=string.split(line)
        timings=parts[10]

        tparts=string.split(timings, '/')
        self.__calls=int(tparts[1])

        self.__calltime=int(tparts[0][:-2])

    def __str__(self):
        """Print a reasonable string version of this thing."""
        return "Log entry:  " + str(self.__timestamp) \
            + " (" + str(self.getTimestampNearest(60)) + ") " \
            + str(self.__calls) + "/" + str(self.__calltime)

    def getTimestamp(self):
        """Get the time at which this log entry occurred."""
        return self.__timestamp

    def getTimestampNearest(self, accuracy):
        """Get the timestamp truncated to the given number of seconds."""
        return( int(self.__timestamp/accuracy) * accuracy)

    def getNumCalls(self):
        return self.__calls

    def getCallTime(self):
        return (self.__calltime / self.__calls)

if __name__ == '__main__':

    # lf=open(sys.argv[1])

    lastTime=0
    totalCalls=0
    totalTime=0

    l=sys.stdin.readline()
    while l != '':
        try:
            le=LogEntry(l)
            # print str(le)

            t=le.getTimestampNearest(60)

            if t!=lastTime and totalCalls > 0:
                print "update " + sys.argv[1] + " " \
                    + string.join(  (str(lastTime),
                                    str(totalCalls),
                                    str(totalTime)), ':')
                totalCalls=0
                totalTime=0

            if le.getNumCalls() > 0:
                lastTime=t
                totalCalls = totalCalls + 1
                totalTime = totalTime + le.getCallTime()

        except:
            e=sys.exc_info()
            # print "Exception with " + l, str(e[1])

        l=sys.stdin.readline()
