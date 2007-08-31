#!/usr/bin/env python
"""

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import sys

# Lame way to find a numer whose last digit is unique
def get_last_digit(nums):
    h={}
    for n in nums:
        if n[-1] in h:
            h[n[-1]].append(n)
        else:
            h[n[-1]] = [n]
    unique=None
    for k,v in h.iteritems():
        if len(v) == 1:
            assert not unique
            unique=v[0]
    assert unique
    return int(unique)

if __name__ == '__main__':
    print "Input numbers:"
    nums=sys.stdin.readline().split()
    last_digit = get_last_digit(nums)
    print "Last digit: ", last_digit

    first_digits = range(last_digit % 4, 40, 4)
    print "Second digits:", first_digits

    if last_digit % 4 < 3:
        next_start = (last_digit % 4) + 2
    else:
        next_start = (last_digit % 4) - 2
    second_digits = range(next_start, 40, 4)
    print "last digits:", second_digits

    for first in first_digits:
        for second in second_digits:
            print "Try ", first, second, last_digit
            sys.stdin.readline()
