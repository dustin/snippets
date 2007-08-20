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

if __name__ == '__main__':
    unittest.main()
