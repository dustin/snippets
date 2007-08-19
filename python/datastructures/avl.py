#!/usr/bin/env python
"""
AVL tree implementation.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import sys

class Node(object):
    """A node in the tree."""

    def __init__(self, value):
        self.value=value

        self.left=None
        self.right=None

    @property
    def balance_factor(self):
        l=r=0
        def rec(node):
            lh=rh=0
            if node.left:
                lh=rec(node.left)
            if node.right:
                rh=rec(node.right)
            return 1 + max(lh, rh)

        if self.left:
            l=rec(self.left)
        if self.right:
            r=rec(self.right)
        return r-l

    def __len__(self):
        def rec(node):
            rv=1
            if node.left:
                rv += rec(node.left)
            if node.right:
                rv += rec(node.right)
            return rv
        return rec(self)

    def __nonzero__(self):
        return True

    def __str__(self):
        return "<Node val=%s balance=%d>" \
            % (str(self.value), self.balance_factor)

class AVLTree(object):
    """An AVL tree."""

    def __init__(self):
        self.root=None

    def add(self, value):
        """Add a value to the node.

        return true if the tree was altered
        """
        if self.root:
            self.__add_at_node(self.root, value)
            self.root = self.__checkRotation(self.root)
        else:
            self.root=Node(value)
            rv=True

        # self.__check_balance(self.root)

    def __check_balance(self, node):
        if node:
            assert abs(node.balance_factor) < 2
            self.__check_balance(node.left)
            self.__check_balance(node.right)

    def __add_at_node(self, node, value):
        offset=0
        # Add a node recursively
        if value == node.value:
            # Already there
            pass
        else:
            if value < node.value:
                offset=-1
                if node.left:
                    self.__add_at_node(node.left, value)
                else:
                    node.left=Node(value)
            else:
                offset=1
                if node.right:
                    self.__add_at_node(node.right, value)
                else:
                    node.right=Node(value)
        if node.left:
            node.left=self.__checkRotation(node.left)
        if node.right:
            node.right=self.__checkRotation(node.right)

    def __checkRotation(self, node):
        rv=node
        if node.balance_factor > 1:
            sys.stderr.write("%s is right heavy\n" % str(node))
            if node.right and node.right.balance_factor < 0:
                # Rotate double left
                node.right = self.__rotate_right(node.right)
                rv = self.__rotate_left(node)
            else:
                # single left
                rv=self.__rotate_left(node)
        elif node.balance_factor < -1:
            sys.stderr.write("%s is left heavy\n" % str(node))
            if node.left and node.left.balance_factor > 0:
                # double right
                node.left = self.__rotate_left(node.left)
                rv = self.__rotate_right(node)
            else:
                # single right
                rv=self.__rotate_right(node)
        return rv

    def __rotate_left(self, node):
        sys.stderr.write("  rotating left at %s\n" % str(node))
        a, b, c = node, node.right, node.right.right
        rv=b
        a.right = b.left
        b.left=a
        sys.stderr.write("  L [%s %s %s]\n     -> [%s %s %s]\n"
            % tuple([str(x) for x in (a, b, c, rv, rv.left, rv.right)]))
        return rv

    def __rotate_right(self, node):
        c, b, a = node, node.left, node.left.left
        rv=b
        c.left=b.right
        b.right=c
        sys.stderr.write("  R [%s %s %s]\n     -> [%s %s %s]\n"
            % tuple([str(x) for x in (c, b, a, rv, rv.left, rv.right)]))
        return rv

    def __len__(self):
        rv=0
        if self.root:
            rv=len(self.root)
        return rv

    def to_dot(self, f=sys.stdout):
        """Convert this tree to a dot file."""

        f.write("digraph avl {\n")

        def printNodeCons(node):
            if node:
                f.write("\t// %s\n" % str(node))
                if node.left:
                    f.write('\t%s -> %s [label = "l"];\n'
                        % (node.value, node.left.value))
                    printNodeCons(node.left)
                if node.right:
                    f.write('\t%s -> %s [label = "r"];\n'
                        % (node.value, node.right.value))
                    printNodeCons(node.right)

        printNodeCons(self.root)

        f.write("}\n")

if __name__ == '__main__':
    t=AVLTree()
    for i in range(128):
        t.add(i)
        sys.stderr.write("**** writing out %d\n" % i)
        f=open("/tmp/test-%02d.dot" % i, "w")
        t.to_dot(f)
        f.close()
        assert abs(t.root.balance_factor) < 2

    sys.stderr.write("root=%s\n" % str(t.root))

    t.to_dot()
