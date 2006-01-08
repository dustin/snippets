#!/usr/bin/env python
"""
Library for commandline stat checking utilities.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 868F43BB-06BD-41D1-8535-D61F2910ADE0

import sys
import getopt
import traceback
import cmsstat

def usage(msg=None):
    if msg is not None:
        sys.stderr.write("*** " + msg.strip() + "\n")
    sys.stderr.write(\
"""Usage:  %s [-l v] [-L v] [-h v] [-H v] host service

target is the ideal rate for this reading in units per minute, and the warn and
crit values are the distance from the target value.

Options:

    -l v  -  low warning value
    -L v  -  low critical value
    -h v  -  high warning value
    -H v  -  high critical value

If you specify a warning or critical option for high or low, you must specify
the other.  I.e. specifying -l without -L will be an error.  Either low values
or high values (or both) must be specified.\n""" % (sys.argv[0],))
    sys.exit(-1)

def checkHighRange(val, warn, crit):
    """Check to make sure a number is below the warning and critical value.

    if val < warn, returns (0, "OK")
    if val < crit, returns (1, "WARNING")
    else, returns (2, "CRITICAL")
    """
    rv, msg=-1, "UNKNOWN"
    if val > crit:
        rv, msg=2, "CRITICAL"
    elif val > warn:
        rv, msg=1, "WARNING"
    else:
        rv, msg=0, "OK"

    return rv, msg

def checkLowRange(val, warn, crit):
    """Check to make sure a number is above the warning and critical value.

    if val > warn, returns (1, "WARNING")
    if val > crit, returns (0, "OK")
    else, returns (2, "CRITICAL")
    """
    rv, msg=-1, "UNKNOWN"
    if val > warn:
        rv, msg=0, "OK"
    elif val > crit:
        rv, msg=1, "WARNING"
    else:
        rv, msg=2, "CRITICAL"

    return rv, msg

def runNagiosCheck(f, stdout=sys.stdout):
    """Run a nagios check function that returns an integer rv and a
    one-line-message, print the message and exit.  If the function throws an
    exception, print the exception in a usable way."""
    rv=-1
    try:
        rv, msg=f()
        stdout.write(msg + "\n")
    except:
        import traceback
        e=sys.exc_info()
        stdout.write(''.join(traceback.format_exception_only(e[0], e[1])))
    sys.exit(rv)

def paramCheck(nm, warn, crit):
    if warn is None and crit is None:
        pass
    elif warn is None and crit is not None:
        usage("Warn value for %s specified without crit" % nm)
    elif warn is not None and crit is None:
        usage("Crit value for %s specified without warn" % nm)

def check(getValFunc, host, service, minWarn, minCrit, maxWarn, maxCrit):
    url="http://%s:8080/admin/monitor/stat" % (host,)

    rate=getValFunc(host, service)

    rv, msg=-1, "UNKNOWN"
    if minWarn is not None and minCrit is not None:
        rv, msg=checkLowRange(rate, minWarn, minCrit)
    if rv < 1 and maxWarn is not None and maxCrit is not None:
        rv, msg=checkHighRange(rate, maxWarn, maxCrit)

    return rv, "%s - %s - rate=%.2f (min=%s, max=%s)" \
            % (msg, service, rate, `(minWarn, minCrit)`, `(maxWarn, maxCrit)`)

def runCheck(getValFunc, argsIn=sys.argv[1:], stdout=sys.stdout):
    """Run a check with the given value check function."""
    try:
        opts, args = getopt.getopt(argsIn, 'l:L:h:H:')
        host, service=args

        minWarn, minCrit, maxWarn, maxCrit=None, None, None, None
        for pair in opts:
            if pair[0]=='-l': minWarn=int(pair[1])
            elif pair[0]=='-L': minCrit=int(pair[1])
            elif pair[0]=='-h': maxWarn=int(pair[1])
            elif pair[0]=='-H': maxCrit=int(pair[1])
    except getopt.GetoptError, e:
        usage(''.join(traceback.format_exception_only(e[0], e[1])))
    except ValueError:
        usage()

    paramCheck("min", minWarn, minCrit)
    paramCheck("max", maxWarn, maxCrit)

    if minWarn is None and maxWarn is None:
        usage("No range specified")

    runNagiosCheck(lambda: check(getValFunc, host, service,
            minWarn, minCrit, maxWarn, maxCrit), stdout)
