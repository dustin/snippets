#!/usr/bin/env jython

import com

import lookupSerial

g=com.twowire.group.Group.getGroup("Global")
print g
g=com.twowire.group.Group.getGroup("global_mstr")
print g
g=com.twowire.group.Group.getGroup(96)
print g
g=com.twowire.group.Group.getGroup(42)
print g

ser=lookupSerial.GatewaySerial('400000000334')
gwb=ser.getGatewayBean()
print gwb.getClass().getName()
print "Version compatibility:  " + `gwb.getVersionCompatibility()`

print "Groups for " + `gwb`
for g in com.twowire.group.Group.getGroups(gwb, 0):
    print g

apps=com.twowire.fileupdate.AppsIni()
print "Expected version:  " + `apps.getExpectedVersion(gwb)`
