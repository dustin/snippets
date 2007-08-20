#!/usr/bin/env python
"""
Unit tests for the avl tree.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import unittest

import avl

class AVLTreeTest(unittest.TestCase):

    def setUp(self):
        self.t=avl.AVLTree(range(10))

    def checkBalance(self, t):
        def __check_balance(node):
            if node:
                assert abs(node.balance_factor) < 2
                __check_balance(node.left)
                __check_balance(node.right)
        __check_balance(t.root)

    def testBalance(self):
        """Check the balance of the test tree."""
        self.checkBalance(self.t)

    def testLen(self):
        """Test the length operation."""
        self.assertEquals(10, len(self.t))

    def testInorder(self):
        """Test in-order traversal."""
        self.assertEquals(list([x for x in range(10)]), list(self.t))

    def testPostOrder(self):
        """Test post-order traversal."""
        self.assertEquals(list(reversed([x for x in range(10)])),
            list(reversed(self.t)))

    def testPreOrder(self):
        """Test pre-order traversal."""
        self.assertEquals([3, 1, 0, 2, 7, 5, 4, 6, 8, 9],
            list(self.t.preorder()))

    def testNodeHeight(self):
        """Test node height calculation - balanced."""
        self.assertEquals(4, self.t.root.compute_height())

        n=avl.Node(3)
        n.left=avl.Node(2)
        n.right=avl.Node(4)
        n.left.left=avl.Node(1)
        n.right.right=avl.Node(5)

        self.assertEquals(3, n.compute_height())

    def testNodeHeightLeftHeavy(self):
        """Test node height calculation - left heavy."""
        n=avl.Node(3)
        n.left=avl.Node(2)
        n.right=avl.Node(4)
        n.left.left=avl.Node(1)

        self.assertEquals(3, n.compute_height())

    def testNodeHeightRightHeavy(self):
        """Test node height calculation - right heavy."""
        n=avl.Node(3)
        n.right=avl.Node(4)
        n.left=avl.Node(1)
        n.right.right=avl.Node(5)

        self.assertEquals(3, n.compute_height())

    def testContains(self):
        """Test contains."""
        for i in range(10):
            self.assertTrue(i in self.t)
        self.assertFalse(-1 in self.t)
        self.assertFalse(10 in self.t)

if __name__ == '__main__':
    unittest.main()
