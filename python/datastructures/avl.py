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
        l=r=0
        if self.left:
            l=len(self.left)
        if self.right:
            r=len(self.right)
        return l + r + 1

    def __nonzero__(self):
        return True

    def __str__(self):
        return "<Node val=%s balance=%d>" \
            % (str(self.value), self.balance_factor)

class AVLTree(object):
    """An AVL tree."""

    _LEFT=-1
    _RIGHT=1

    def __init__(self, vals=[]):
        self.root=None
        for v in vals:
            self.add(v)

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

    def inorder(self):
        """Visit every node in the tree in order (default iterator)."""
        def rec(node):
            if node:
                for n in rec(node.left):
                    yield n
                yield node.value
                for n in rec(node.right):
                    yield n
        return rec(self.root)

    def postorder(self):
        """Visit every node in the tree in reverse order (reversed iterator)."""
        def rec(node):
            if node:
                for n in rec(node.right):
                    yield n
                yield node.value
                for n in rec(node.left):
                    yield n
        return rec(self.root)

    def preorder(self):
        """Visit every node in the tree in tree  order."""
        def rec(node):
            if node:
                yield node.value
                for n in rec(node.left):
                    yield n
                for n in rec(node.right):
                    yield n
        return rec(self.root)

    __iter__ = inorder

    __reversed__ = postorder

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
            if node.right and node.right.balance_factor < 0:
                # Rotate double left
                node.right = self.__rotate(node.right, self._RIGHT)
                rv = self.__rotate(node, self._LEFT)
            else:
                # single left
                rv=self.__rotate(node, self._LEFT)
        elif node.balance_factor < -1:
            if node.left and node.left.balance_factor > 0:
                # double right
                node.left = self.__rotate(node.left, self._LEFT)
                rv = self.__rotate(node, self._RIGHT)
            else:
                # single right
                rv=self.__rotate(node, self._RIGHT)
        return rv

    def __rotate(self, node, direction):
        assert direction == self._LEFT or direction == self._RIGHT
        if direction == self._LEFT:
            a, b, c = node, node.right, node.right.right
            a.right = b.left
            b.left=a
        elif direction == self._RIGHT:
            c, b, a = node, node.left, node.left.left
            c.left=b.right
            b.right=c
        return b

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

    sys.stderr.write("root=%s\n" % str(t.root))

    t.to_dot()
