#!/usr/bin/env jython

import getdb

import java
import com

if __name__ == '__main__':

    files=(com.twowire.fileupdate.AppsIni(),
        com.twowire.fileupdate.BaseFirewallRules(),
        com.twowire.fileupdate.FirewallMonitorRules())

    mf=com.twowire.fileupdate.MatrixSingleton.getInstance()

    for groupSpec in ('global_mstr', 96):

        print "Working on", groupSpec

        g=com.twowire.group.Group.getGroup(groupSpec)

        print "Group: ", g

        for filetype in files:

            matrix=mf.getMatrix(filetype, g)

            print "\tMatrix for", filetype, ":", matrix

            for n in range(44,47):
                print "\t\tFetching", n
                o=matrix.find(java.lang.Integer(n))
                print "\t\t", o
                if o is not None:
                    print "\t\t\t (",o.getCarryOn(), ")"

    print "All matrices:"
    print mf.getMatrixList()
