#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: C57998D6-32B3-11D9-93E7-000393CFE6B8

import random
import unittest
import shortestpath

class StringNode(shortestpath.Node):
    def __init__(self, s):
        shortestpath.Node.__init__(self)
        self.s=s

    def __repr__(self):
        return "<StringNode: " + self.s + ">"

class ShortestPathTestCase(unittest.TestCase):

    def setUp(self):
        self.a=StringNode("A")
        self.b=StringNode("B")
        self.c=StringNode("C")
        self.d=StringNode("D")
        self.e=StringNode("E")
        self.f=StringNode("F")
        self.g=StringNode("G")

        self.nodes=dict([("A", self.a), ("B", self.b), ("C", self.c),
            ("D", self.d), ("E", self.e), ("F", self.f), ("G", self.g)])

        self.a.linkTo(self.b)
        self.a.linkTo(self.c, 15)

        self.b.linkTo(self.c)

        self.c.linkTo(self.d)
        self.c.linkTo(self.e)
        self.c.linkTo(self.f)

        self.d.linkTo(self.c, 100)

        self.e.linkTo(self.e, 10)

        self.f.linkTo(self.g, 10)
        self.f.linkTo(self.b, 200)

        shortestpath.calculatePaths(self.nodes.values())

    def assertLinkMatch(self, a, b, expected, cost):
        nextHop=a.getNextHop(b)
        if expected is None:
            self.failUnless(nextHop is None,
                msg="Expected no link from " + `a` + " to " + `b`)
        else:
            self.failIf(nextHop is None,
                msg="Expected a link from " + `a` + " to " + `b`
                    + " via " + `expected`)
            self.failUnless(nextHop.getTo() is expected,
                "Expected link from " + `a` + " to " + `b` + " via "
                    + `expected` + ", but got " + `nextHop.getTo()`)
            self.assertEquals(cost, nextHop.cost,
                "Incorrect cost for " + `a` + " -> " + `b`
                + "(" + `cost` + " != " + `nextHop.cost` + ")")

    def testSPFind(self):
        """Basic SP find."""
        self.assertLinkMatch(self.a, self.a, None, 0)
        self.assertLinkMatch(self.a, self.b, self.b, 10)
        self.assertLinkMatch(self.a, self.c, self.c, 15)
        self.assertLinkMatch(self.a, self.d, self.c, 25)
        self.assertLinkMatch(self.a, self.e, self.c, 25)
        self.assertLinkMatch(self.a, self.f, self.c, 25)
        self.assertLinkMatch(self.a, self.g, self.c, 35)

        self.assertLinkMatch(self.b, self.a, None, 0)
        self.assertLinkMatch(self.b, self.b, self.c, 220)
        self.assertLinkMatch(self.b, self.c, self.c, 10)
        self.assertLinkMatch(self.b, self.d, self.c, 20)
        self.assertLinkMatch(self.b, self.e, self.c, 20)
        self.assertLinkMatch(self.b, self.f, self.c, 20)
        self.assertLinkMatch(self.b, self.g, self.c, 30)

        self.assertLinkMatch(self.c, self.a, None, 0)
        self.assertLinkMatch(self.c, self.b, self.f, 210)
        self.assertLinkMatch(self.c, self.c, self.d, 110)
        self.assertLinkMatch(self.c, self.d, self.d, 10)
        self.assertLinkMatch(self.c, self.e, self.e, 10)
        self.assertLinkMatch(self.c, self.f, self.f, 10)
        self.assertLinkMatch(self.c, self.g, self.f, 20)

        self.assertLinkMatch(self.d, self.a, None, 0)
        self.assertLinkMatch(self.d, self.b, self.c, 310)
        self.assertLinkMatch(self.d, self.c, self.c, 100)
        self.assertLinkMatch(self.d, self.d, self.c, 110)
        self.assertLinkMatch(self.d, self.e, self.c, 110)
        self.assertLinkMatch(self.d, self.f, self.c, 110)
        self.assertLinkMatch(self.d, self.g, self.c, 120)

        self.assertLinkMatch(self.e, self.a, None, 0)
        self.assertLinkMatch(self.e, self.b, None, 0)
        self.assertLinkMatch(self.e, self.c, None, 0)
        self.assertLinkMatch(self.e, self.d, None, 0)
        self.assertLinkMatch(self.e, self.e, self.e, 10)
        self.assertLinkMatch(self.e, self.f, None, 0)
        self.assertLinkMatch(self.e, self.g, None, 0)

        self.assertLinkMatch(self.f, self.a, None, 0)
        self.assertLinkMatch(self.f, self.b, self.b, 200)
        self.assertLinkMatch(self.f, self.c, self.b, 210)
        self.assertLinkMatch(self.f, self.d, self.b, 220)
        self.assertLinkMatch(self.f, self.e, self.b, 220)
        self.assertLinkMatch(self.f, self.f, self.b, 220)
        self.assertLinkMatch(self.f, self.g, self.g, 10)

        self.assertLinkMatch(self.g, self.a, None, 0)
        self.assertLinkMatch(self.g, self.b, None, 0)
        self.assertLinkMatch(self.g, self.c, None, 0)
        self.assertLinkMatch(self.g, self.d, None, 0)
        self.assertLinkMatch(self.g, self.e, None, 0)
        self.assertLinkMatch(self.g, self.f, None, 0)
        self.assertLinkMatch(self.g, self.g, None, 0)

    def testShortestPath(self):
        gp=shortestpath.getShortestPath
        sp=gp(self.a, self.b)
        self.assertEquals(1, len(sp), "Shortest path from A -> B")
        try:
            sp=gp(self.a, self.a)
            self.fail("Expected to not find a path from A -> A, found " + `sp`)
        except shortestpath.NoPathException:
            pass
        sp=gp(self.a, self.c)
        self.assertEquals(1, len(sp), "ShortestPath from A -> C:  " + `sp`)
        sp=gp(self.a, self.d)
        self.assertEquals(2, len(sp), "ShortestPath from A -> D:  " + `sp`)
        sp=gp(self.a, self.e)
        self.assertEquals(2, len(sp), "ShortestPath from A -> E:  " + `sp`)

        sp=gp(self.d, self.e)
        self.assertEquals(2, len(sp), "ShortestPath from D -> E:  " + `sp`)

        sp=gp(self.e, self.e)
        self.assertEquals(1, len(sp), "ShortestPath from E -> E:  " + `sp`)

    def testBigGraph(self):
        nodes=[]
        # Generate one hundred nodes
        for i in range(100):
            nodes.append(StringNode(str(i)))

        # Randomly select 50 nodes to link each to 50 other nodes
        r=random.Random()
        for i in r.sample(range(100), 50):
            node=nodes[i]
            for j in r.sample(range(100), 50):
                node.linkTo(nodes[j])

        shortestpath.calculatePaths(nodes)

if __name__ == '__main__':
    unittest.main()
