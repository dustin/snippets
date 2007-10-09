#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>

Check the raid status on purple.
"""

import sys
import posix
import traceback

class BadDeviceState:
    def __init__(self, msg):
        self.msg=msg

    def __repr__(self):
        return "<BadDeviceState: " + self.msg + ">"

def checkStat(raidstat):
    devLines=filter(lambda s: s.find("SPYRAID") > 0, raidstat)
    assert(len(devLines) == 4)

    for l in devLines:
        a=l.split()
        state=a[-2]
        if state != 'Normal':
            raise BadDeviceState(state)

if __name__ == '__main__':
    f=posix.popen("/sbin/sysctl hpt374.status")
    raidstat=f.readlines()
    f.close()

    try:
        checkStat(raidstat)
    except:
        traceback.print_exc()
        if raidstat is not None:
            sys.stderr.write("\n\n RAID Status:\n\n")
            sys.stderr.writelines(raidstat)
