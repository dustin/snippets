#!/usr/bin/env python
"""
Find the shortest path between two nodes.

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""

import weakref
import exceptions

class Vertex(object):
    """A vertex (edge) between two nodes."""

    DEFAULT_COST=10

    def __init__(self, to, cost=DEFAULT_COST):
        self._to=weakref.ref(to)
        self.cost=cost

    @property
    def to(self):
        return self._to()

    def __repr__(self):
        return "<Vertex cost=" + `self.cost` + ", to=" + `self.to` + ">"

    def __cmp__(self, other):
        rv=0
        if self.cost > other.cost:
            rv=1
        elif self.cost < other.cost:
            rv=-1
        else:
            rv=cmp(self.to, other.to)
        return rv

class Node(object):
    """A node with connections."""
    def __init__(self):
        self.nextHops={}
        self.connections=[]

    def getNextHop(self, to):
        """Get the next hop in the path to the given node.

           return None if there's no such hop."""

        rv=None
        if to in self.nextHops:
            rv=self.nextHops[to]
        return rv

    def linkTo(self, n, cost=Vertex.DEFAULT_COST):
        """Create a direct link from this node to the given node at the
           given cost."""
        self.connections.append(Vertex(n, cost))

class NoPathException(exceptions.Exception):
    """Exception thrown when attempting to find the shortest path between two
       nodes when there is no matching path."""

    def __init__(self, f, t):
        self.nodeFrom=f
        self.nodeTo=t

### Main stuff

def __addHopFrom(node, dest, next, cost):
    # If we already have a mapping, only add this one if it costs less
    if dest in node.nextHops:
        if cost < node.nextHops[dest].cost:
            node.nextHops[dest] = Vertex(next, cost)
    else:
        # Otherwise, just add it
        node.nextHops[dest] = Vertex(next, cost)

def __recordLink(node, cost, nextHop, other, s):
    # Skip if we've already done this one
    if not other in s:
        s[other]=True

        __addHopFrom(node, other, nextHop, cost)

        for vertex in other.connections:
            nextCost = cost + vertex.cost
            thisNode=vertex.to
            __recordLink(node, nextCost, nextHop, thisNode, s)

def __calculatePathForNode(node):
    """Calculate the paths for the given node."""
    node.nextHops={}
    for v in node.connections:
        __recordLink(node, v.cost, v.to, v.to, {})

def calculatePaths(nodes):
    """Calculate all the paths for the given collection of nodes."""
    for n in nodes:
        __calculatePathForNode(n)

def getShortestPath(nodeFrom, nodeTo):
    """Get the shortest path between two nodes."""
    rv=[]
    v = nodeFrom.getNextHop(nodeTo)
    if v is None:
        raise NoPathException(nodeFrom, nodeTo)

    current = v.to
    while current is not nodeTo:
        rv.append(current)

        v=current.getNextHop(nodeTo)
        current = v.to

    rv.append(current)

    return rv
