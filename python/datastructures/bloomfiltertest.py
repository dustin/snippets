#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

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
        """Test a simple string add to the bloom filter."""
        assert not "something" in self.bf
        self.bf.add("something")
        assert "something" in self.bf

    def testHashGeneration(self):
        """Validate that we will generate up to 100 unique hash functions."""
        hf=self.bf._generateHashFunctions(100)
        self.failUnlessEqual(len(hf), 100)

    def __performTestWithSequence(self, seq):
        found=[]
        for s in seq:
            if s in self.bf:
                found.append(s)
            self.bf.add(s)
        # Make sure there weren't too many false positives
        self.failUnless(len(found) < 2, "Too many false positives " + `found`)
        for s in seq:
            assert s in self.bf

    def testIntegers(self):
        """Test a bloom filter with integers instead of strings"""
        self.__performTestWithSequence(range(50))

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
        self.__performTestWithSequence(words)

if __name__ == '__main__':
    unittest.main()
