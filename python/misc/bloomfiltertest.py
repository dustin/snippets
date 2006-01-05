#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 082BCAFF-0822-4CAC-8313-B55C5B5F1A7D

import unittest

import bloomfilter

class BitArrayTest(unittest.TestCase):
    """Bit array operations"""

    def setUp(self):
        self.bf=bloomfilter.BitArray(128)

    def testPotentiallyNegative(self):
        """Test a number that works out to be negative."""
        assert not self.bf.isSet(31)
        self.bf.set(31)
        assert self.bf.isSet(31)

    def testARange(self):
        """Test all of the bit operations from 0 to 128"""
        for i in range(128):
            assert not self.bf.isSet(i)
            self.bf.set(i)
            assert self.bf.isSet(i)
            self.bf.clear(i)
            assert not self.bf.isSet(i)

class BloomFilterTest(unittest.TestCase):
    """Bloom filter operation tests"""

    def setUp(self):
        self.bf=bloomfilter.BloomFilter(50)

    def testAdd(self):
        assert not "something" in self.bf
        self.bf.add("something")
        assert "something" in self.bf

    def testBigAdd(self):
        """Test checking and adding 50 words in a 50 word bloom filter"""
        words="""uncriticised
confervoid
homozygosis
overnationalization
divergency
Choes
insecticide
rerig
brougham
travestiment
aciculum
oviculum
juristic
retinoblastoma
fourche
disport
unendowed
frontoethmoid
unsteepled
tritactic
enterography
peculiarsome
laparoelytrotomy
brancard
transversally
ingotman
recommencement
theophanism
tryptone
matchlessness
anneloid
authorcraft
elongate
subdelirium
interloculus
actinoblast
unchurchly
oystershell
imaginableness
genteelize
ovoplasm
maycock
pseudoanatomic
Eleaticism
zingiberene
intercurrent
betread
tetronic
Saxonish
Isthmia""".split("\n")
        found=[]
        for w in words:
            if w in self.bf:
                found.append(w)
            self.bf.add(w)
        # Make sure there weren't too many false positives
        assert len(found) < 2
        for w in words:
            assert w in self.bf

if __name__ == '__main__':
    unittest.main()
