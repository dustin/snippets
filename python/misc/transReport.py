#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: transReport.py,v 1.2 2003/08/15 02:44:44 dustin Exp $
"""

import exceptions
import random

class MuxProtocol:

    def __init__(self):
        pass

    def __logIn(self, s):
        print "<<< " + s

    def __logOut(self, s):
        print ">>> " + s

    def getSettings(self, a):
        """Returns a dict of settings"""
        self.__logOut("getSettings(" + `a` + ")")
        self.__logIn("getSettings response")

    def setSettings(self, h):
        """Takes a dict of settings"""
        self.__logOut("setSettings(" + `h` + ")")
        self.__logIn("setSettings response")

    def authenticate(self):
        """Authenticate the gateway"""
        self.__logOut("authenticate request");
        self.__logIn("authenticate response");

class FlowException:
    pass

class AutoUpgradeException(FlowException):

    def __repr__(self):
        return "AutoUpgrade in progress"

class Transaction:

    def __init__(self):
        self.settingsSeen={}
        self.optionList=[]
        self.r=random.Random()
        self.mux=MuxProtocol()

    def logOut(self, s):
        print ">>> " + s
    def logIn(self, s):
        print "<<< " + s

    def go(self):
        """Subclasses override"""
        raise exceptions.NotImplementedError()

    def debug(self, s):
        print "DBG:  " + s
        pass

    def getSettings(self, setList):
        self.debug("wanting to get settings:  " + `setList`)
        newSetList=[]
        for s in setList:
            if not self.settingsSeen.has_key(s):
                newSetList.append(s)
        self.mux.getSettings(newSetList)
        for s in setList:
            self.settingsSeen[s]=1

    def setSettings(self, setList):
        self.debug("wanting to set settings:  " + `setList`)
        newSetList={}
        for k,v in setList.iteritems():
            if not self.settingsSeen.has_key(k):
                newSetList[k]=v
                self.settingsSeen[k]=v
        self.mux.setSettings(newSetList)

    def preExecute(self):
        self.debug("STARTING")

    def postExecute(self):
        self.debug("FINISHING")
        self.debug("Settings seen:  " + `self.settingsSeen`)

    def execute(self):
        self.preExecute()
        self.go()
        self.postExecute()

    def autoUpgrade(self):
        raise AutoUpgradeException()

class Heartbeat(Transaction):
    pass

class HeartbeatPeriodic(Heartbeat):

    def __init__(self, count=0):
        Transaction.__init__(self)
        self.count=count

    def __doIfMod(self, n, op):
        if self.count == 0 or self.count % n == 0:
            op()

    def __doIfNotMod(self, n, op):
        if self.count == 0 or self.count % n != 0:
            op()

    def __reorg(self):
        self.debug("Performing a reorg")

    def __regroup(self):
        self.debug("Performing a regroup")

    def __resetting(self):
        self.debug("Performing a resettingification")
        settings={}
        for sid in range(50):
            settings['GW.randsetting.' + `sid`] = 'someValue'
        self.setSettings(settings)

    def __getAllOptions(self):
        self.debug("Getting all options from the gateway")
        self.logOut("getOptions()")
        self.logIn("getOptions response")
        for i in range(6):
            self.optionList.append('Option ' + `i`)

    def __enableBBA(self):
        self.debug("Enabling BBAs")
        self.logOut("setVoucher()")
        self.logIn("setVoucher response")

    def __updateFirewallRules(self):
        self.debug("Updating firewall rules")
        self.getSettings(('GW.FW.Version',))
        self.setSettings({'GW.FW.NewVersionAvailable':1})

    def __updateAppsIni(self):
        self.debug("Updating apps.ini")
        self.getSettings(('GW.Apps.Version',))
        self.setSettings({'GW.Apps.NewVersionAvailable':1})

    def __updateHBSchedule(self):
        self.debug("Updating heartbeat schedule")
        self.setSettings({'GW.HB.Time': self.r.randint(1,1440)})

    def __updateDDNSSetting(self):
        self.debug("Updating the DDNS setting")
        self.setSettings({'GW.DDNS.Hostname': 'my.hostname.com'})

    def __updateGateway(self):
        self.__enableBBA()
        self.__updateFirewallRules()
        self.__updateAppsIni()
        self.__updateHBSchedule()
        self.__updateDDNSSetting()

    def __showUpgradeCommand(self):
        self.debug("Set the upgrade URL")
        self.setSettings({'Gw.UI.UpgradeButtonURL': 'http://something'})

    def preExecute(self):
        Transaction.preExecute(self)
        self.logIn("Heartbeat()")

    def postExecute(self):
        self.logOut("Heartbeat response")
        Transaction.preExecute(self)

    def go(self):
        self.debug("Performing a HeartbeatPeriodic, number " + `self.count`)
        # self.autoUpgrade()

        self.getSettings(('Gw.Unit.Wireless',
            'Gw.Unit.WirelessCapability', 'Gw.Unit.PhonePort',
            'Gw.UI.ShowEmailAlert',))
        self.__doIfMod(7, self.__reorg)
        self.__doIfMod(17, self.__regroup)
        self.__doIfMod(31, self.__resetting)
        self.__showUpgradeCommand()
        self.__doIfMod(2, self.__getAllOptions)
        self.__doIfMod(2, self.__updateGateway)

if __name__ == '__main__':
    for i in range(40):
        hb=HeartbeatPeriodic(i)
        try:
            hb.execute()
        except FlowException, e:
            print `e`
        print "--------"
