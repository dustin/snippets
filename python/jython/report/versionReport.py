#!/usr/bin/env jython

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

def dotify(compatibilities):
    f=open('/tmp/dotify/versionmap.dot', 'w')
    dotifyVersionMap(compatibilities, f)
    f.close()

    # Next hop maps
    for vc in compatibilities.values():
        n=vc.getName()
        f=open('/tmp/dotify/' + n + '.dot', 'w')
        dotifyMappings(compatibilities, vc, f)
        f.close()
        f=open('/tmp/dotify/' + n + '-rev.dot', 'w')
        dotifyRevMap(compatibilities, vc, f)
        f.close()

    f=open('/tmp/dotify/versions.tex', 'w')
    createVersionIndex(compatibilities, f)
    f.close()

def createVersionIndex(compatibilities, tofile=sys.stdout):

    vcs=compatibilities.values()
    vcs.sort()
    for vc in vcs:
        # Get the list of versions in this compatibility range
        a=[]
        v=vc.getVersions()
        i=v.iterator()
        while i.hasNext():
            a.append(i.next().getVersion())
        # sort it
        a.sort()
        if len(a) > 0:
            tofile.write("\\subsection{" + vc.getName() + "}\n")
            tofile.write("\\begin{list}{}\n")
            for v in a:
                tofile.write("\\item " + v.replace("_", "\\_") + "\n")

            tofile.write("\\end{list}\n\n")
        else:
            sys.stderr.write("WARNING!  No versions in compat "
                + vc.getName() + "\n");


def dotifyMappings(compatibilities, vc, tofile=sys.stdout):
    tofile.write("digraph " + `vc.getId()` + " {\n")
    tofile.write('\tpage="8.5,11";\n')
    tofile.write('\tsize="8.5,11";\n')
    tofile.write("\trankdir=LR;\n")

    tofile.write('\t"' + vc.getName() + '";')

    seen={}

    for ovc in compatibilities.values():
        if vc.getName() != ovc.getName():
            nh=vc.getNextHop(ovc)
            if nh is not None:
                sp=net.spy.util.ShortestPath(vc, ovc)
                op=vc
                for p in sp:
                    k=`op.getName()` + "-to-" + `p.getName()`
                    if not seen.has_key(k):
                        tofile.write('\t"' + op.getName() + '" -> "' \
                            + p.getName() + '";\n')
                        seen[k]=1
                    op=p

    tofile.write("}\n")

def dotifyRevMap(compatibilities, vc, tofile=sys.stdout):
    tofile.write("digraph " + `vc.getId()` + " {\n")
    tofile.write('\tpage="8.5,11";\n')
    tofile.write('\tsize="8.5,11";\n')
    tofile.write("\trankdir=LR;\n")

    tofile.write('\t"' + vc.getName() + '";')

    seen={}

    for ovc in compatibilities.values():
        if vc.getName() != ovc.getName():
            # Figure out if we can get to the one destination from here
            nh=ovc.getNextHop(vc)
            if nh is not None:
                # We can reach it, find the path
                sp=net.spy.util.ShortestPath(ovc, vc)
                op=ovc
                for p in sp:
                    k=`op.getName()` + "-to-" + `p.getName()`
                    if not seen.has_key(k):
                        tofile.write('\t"' + op.getName() + '" -> "' \
                            + p.getName() + '";\n')
                        seen[k]=1
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
        tofile.write('[' + vc.getName() + ' (' + `vc.getId()` + ')]\"];\n')

    # Now draw the links
    for vc in compatibilities.values():
        for conn in vc.getConnections():
            l=conn.getTo().getName()
            cost=conn.getCost()
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

