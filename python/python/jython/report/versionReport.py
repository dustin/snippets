#!/usr/bin/env jython

import getdb
import sys
import os
import net
import com
import java

class VersionCompatibility(com.twowire.compatibility.VersionCompatibility):

    def __init__(self, compnum, name):
        com.twowire.compatibility.VersionCompatibility.__init__(self, \
            compnum, name)

    def linkList(self):
        rv=[]

        for c in self.getConnections():
            rv.append(c.getTo().getName())

        return rv

def getPrintableList(l):
    rv=l
    if len(l) > 1000:
        rv=l[0:9]
        rv.append("...")
    return rv

def dotify(compatibilities):
    f=open('/tmp/dotify/versionmap.dot', 'w')
    dotifyVersionMap(compatibilities, f)
    f.close()

    # Next hop maps
    for vc in compatibilities.values():
        n=vc.getName()
        f=open('/tmp/dotify/' + n + '.dot', 'w')
        dotifyMappings2(compatibilities, vc, f)
        f.close()

def dotifyMappings(compatibilities, vc, tofile=sys.stdout):
    tofile.write("digraph " + `vc.getId()` + " {\n")
    tofile.write("\trankdir=LR;\n")
    tofile.write('\tpage="8.5,11";\n')
    tofile.write('\tsize="8.5,11";\n')

    for ovc in compatibilities.values():
        if vc.getName() != ovc.getName():
            nh=vc.getNextHop(ovc)
            if nh is not None:
                frm=vc.getName()
                to=ovc.getName()
                via=nh.getTo().getName()
                tofile.write('\t"' + frm + '" -> "' + to + '";\n');
                tofile.write('\t"' + to \
                    + '" [label="' + to \
                    + ' via ' + via + '"];\n')

    tofile.write("}\n")

def dotifyMappings2(compatibilities, vc, tofile=sys.stdout):
    tofile.write("digraph " + `vc.getId()` + " {\n")
    tofile.write('\tpage="8.5,11";\n')
    tofile.write('\tsize="8.5,11";\n')
    tofile.write("\trankdir=LR;\n")

    tofile.write('\t"' + vc.getName() + '";')

    for ovc in compatibilities.values():
        if vc.getName() != ovc.getName():
            nh=vc.getNextHop(ovc)
            if nh is not None:
                sp=net.spy.util.ShortestPath(vc, ovc)
                op=vc
                for p in sp:
                    tofile.write('\t"' + op.getName() + '" -> "' \
                        + p.getName() + '";\n')
                    op=p

    tofile.write("}\n")


def dotifyVersionMap(compatibilities, tofile=sys.stdout):
    tofile.write("digraph versionMap {\n")
    tofile.write('\tpage="8.5,11";\n')
    tofile.write('\tsize="8.5,11";\n')

    # Create the nodes
    for vc in compatibilities.values():
        tofile.write('\t"' + vc.getName() + '" [shape=box,')
        tofile.write('fontsize=6,label="')
        tofile.write('[' + vc.getName() + ' (' + `vc.getId()` + ')]\\n')
        # Get the list of versions in this compatibility range
        a=[]
        v=vc.getVersions()
        i=v.iterator()
        while i.hasNext():
            a.append(i.next().getVersion())
        # sort it
        a.sort()
        # shrink it
        a=getPrintableList(a)
        for i in range(len(a)):
            tofile.write(a[i] + ' ')
            if i>0 and (i%3 == 0):
                tofile.write('\\n')
        tofile.write("\"];\n")

    # Now draw the links
    for vc in compatibilities.values():
        for conn in vc.getConnections():
            l=conn.getTo().getName()
            cost=conn.getCost()
            if l != vc.getName():
                tofile.write('\t"' + vc.getName() + '" -> "' + l \
                    + '" [fontsize="6",label="' + `cost` + '"];\n')

    tofile.write("}\n")

if __name__ == '__main__':

    if not os.path.isdir('/tmp/dotify'):
        os.mkdir('/tmp/dotify')

    compatibilities={}

    # Shortcut to VersionCompatibility
    VersionCompatibility=com.twowire.compatibility.VersionCompatibility

    i=VersionCompatibility.getAllVersionCompatibilities().iterator()
    while i.hasNext():
        vc=i.next()
        compatibilities[vc.getId()]=vc

    dotify(compatibilities)

