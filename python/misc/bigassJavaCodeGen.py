#!/usr/bin/env python
"""

Copyright (c) 2006  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 105E44C8-1C73-403A-8928-B79215337CFD

import sys

def makeInterfaceName(c, i):
    return "C" + c + "Of" + i

def makeMethodName(c, i):
    return "m" + c + "Of" + i

if __name__ == '__main__':

    sup="Object"

    interfaces=[]

    for c in [str(x) for x in range(1, 11)]:
        ints=[str(x) for x in range(1, 101)]

        cname="Class" + c

        # Declare all of the interfaces
        for i in ints:
            print "interface " + makeInterfaceName(c, i) + " {"
            print "\tvoid " + makeMethodName(c, i) + "();"
            print "}"
        print ""

        # Declare the class in its hierarchy

        print "class " + cname + " extends " + sup
        print "\timplements " \
            + ",\n\t".join([makeInterfaceName(c, x) for x in ints]) + " {"
        for mn in [makeMethodName(c, x) for x in ints]:
            print "\tpublic void " + mn + "() {"
            print "\t}"
        print "}"
        print ""

        interfaces.extend([(c, x) for x in ints])

        sup=cname

    print """
public class TestInsanity {
    private static void printTime(String mname, long start) {
        long dur=System.currentTimeMillis() - start;
        System.out.println(dur + " " + mname);
    }

    public static void main(String[] args) throws Exception {
        %(sup)s o=new %(sup)s();
        int iters=Integer.parseInt(args[0]);
""" % {'sup': sup}

    for c in [str(x) for x in range(1, 11)]:
        print "        test" + c + "(iters, o);"
    print "    }"

    for c in [str(x) for x in range(1, 11)]:
        print "    private static void test" + c + "(int iters, " + sup + " o) {"
        for (c2,i) in [x for x in interfaces if x[0] == c]:
            m=makeMethodName(c, i)
            iname=makeInterfaceName(c, i)
            print "        {"
            print "            long start=System.currentTimeMillis();"
            print "            for(int i=0; i<iters; i++) {"
            print "                o." + m + "();"
            print "            }"
            print "            printTime(\"" + m + "-direct\", start);"
            print "            " + iname + " intf = o;";
            print "            start=System.currentTimeMillis();"
            print "            for(int i=0; i<iters; i++) {"
            print "                intf." + m + "();"
            print "            }"
            print "            printTime(\"" + m + "-indirect\", start);"
            print "        }"
        print "    }"
        print ""

    print """
}"""
