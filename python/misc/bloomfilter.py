#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 44371438-8BD7-488E-A626-10A5BF86F51A

import sys
import sets
import array
import math
import md5
import random
import struct

class BitArray(object):
    """A bit field"""

    # Had to make this 31 due to the stupid signed in problem.
    STORAGE_SIZE=31

    def __init__(self, size):
        self.size=size
        self.storage=array.ArrayType('I')
        for i in range(int(math.ceil(float(size)/float(self.STORAGE_SIZE)))):
            self.storage.append(0)

    def isSet(self, which):
        """Return True if a given bit is set."""
        return ((self.storage[which/self.STORAGE_SIZE] & \
            (1 << (which % self.STORAGE_SIZE))) != 0)

    def set(self, which):
        """Set a bit"""
        self.storage[which/self.STORAGE_SIZE] |= \
            (1 << (which % self.STORAGE_SIZE))

    def clear(self, which):
        """Clear a bit"""
        self.storage[which/self.STORAGE_SIZE] &= \
            (~ (1 << (which % self.STORAGE_SIZE)))

class BloomFilter(object):
    """A bloom filter"""

    def __init__(self, numkeys, error_rate=0.01):
        """Initialize a bloom filter with the given number of keys and
        desired error rate."""

        self.numkeys=numkeys
        self.error_rate=error_rate
        self.size, k=self.__calculate_length_and_keys(numkeys, error_rate)
        self.array=BitArray(self.size)
        self.hashes=self._generateHashFunctions(k)

    def _generateHashFunctions(self, k):
        """Generate k hash functions"""
        r=random.Random()
        # Seed it with some arbitrary number to make it stable
        r.seed(16369)

        # Get some random values for our hash function
        maxv=int("7fffffff", 16)
        hashes=sets.Set([r.randint(0, maxv) for i in range(k)])
        assert len(hashes) == k

        # This is the stub of the hash function
        def doHash(h, key):
            if isinstance(key, str):
                m=md5.md5(key)
                d=m.digest()
                integers=struct.unpack("4l", d)
                return reduce(lambda i, v: i ^ v, integers, h)
            else:
                return (hash(key) ^ h)
 
        return [lambda key: doHash(h, key) for h in hashes]

    def __repr__(self):
        return "<BloomFilter m=" + `self.size` + ", n=" + `self.numkeys` \
            + ", k=" + `len(self.hashes)` \
            + ", desired error rate: " + `self.error_rate` + ">"

    # Pretty blatantly stolen from Bloom::Filter
    def __calculate_length_and_keys(self, num_keys, error_rate):
        best_k=1
        lowest_m=None
        for k in range(1,100):
            m=float(-1 * k * num_keys) \
                / (math.log(1.0 - pow(error_rate, 1.0/k)))
            if lowest_m is None or m < lowest_m:
                lowest_m = m
                best_k = k
        lowest_m = int(lowest_m) + 1
        return lowest_m, best_k

    def __getHashedVals(self, key):
        return [(h(key) % self.size) for h in self.hashes]

    def contains(self, el):
        """True if this bloom filter probably contains the given value."""
        return reduce(lambda i, v: i and v,
            [self.array.isSet(x) for x in self.__getHashedVals(el)], True)

    __contains__ = contains

    def add(self, el):
        """Add the given value to the bloom filter."""
        for h in self.__getHashedVals(el):
            self.array.set(h)

if __name__ == '__main__':
    unittest.main()
