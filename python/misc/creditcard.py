#!/usr/bin/env python
"""
Credit card tools.

Note that there is not support for JCB or Enroute.  I don't know what these
are, and the discovery rules conflict with Amex and each other.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import sys
import string
import doctest
import exceptions

class InvalidCard(exceptions.Exception):
    """Exception thrown when an invalid card number is given."""

class CreditCard(object):
    """Base class for all credit cards."""

    _validLengths=[16]

    def __init__(self, s):
        # Not supposed to use this one directly
        assert self.__class__ != CreditCard
        self.num=s
        self.__validate()

    def __validate(self):
        if len(self.num) not in self._validLengths:
            raise InvalidCard("Invalid card length " + `len(self.num)`
                + " expected one of "
                + ', '.join([str(x) for x in self._validLengths]))
        self._verifyMod10()

    def _verifyMod10(self):
        sum=0
        cur=0
        # Validate the number backwards
        for i in range(len(self.num), 0, -1):
            num=int(self.num[i-1])

            if cur % 2 == 0:
                sum += num
            else:
                # The sum of the digits of the number times two
                sum += reduce(lambda x, y: x+int(y), str(num*2), 0)
            cur+=1

        if sum % 10 != 0:
            raise InvalidCard("Checksum failed")

    def getMangledNum(self):
        """Get a mangled version of the credit card number"""
        return ("X" * (len(self.num)-4)) + self.num[-4:]

class MasterCard(CreditCard):
    """MasterCard type."""

    _validLengths=[16, 13]

class Visa(CreditCard):
    """Visa type."""

    _validLengths=[16, 13]

class Amex(CreditCard):
    """American Express."""

    _validLengths=[15]

class Diner(CreditCard):
    """Diner's club."""

    _validLengths=[14]

class Discover(CreditCard):
    """Discover card."""

PREFIXES={"51": MasterCard, "52": MasterCard, "53": MasterCard,
    "54": MasterCard, "55": MasterCard,
    "4": Visa,
    "34": Amex, "35": Amex, "37": Amex,
    "300": Diner, "301": Diner, "302": Diner, "303": Diner, "304": Diner,
    "305": Diner, "36": Diner, "38": Diner,
    "6011": Discover}

def normalize(ccNumString):
    """Remove all of the non-numbers from the given strings.

    >>> normalize('a-b c235x85')
    '23585'
    """
    allChars=string.maketrans("", "")
    badchars=string.translate(allChars, allChars, string.digits)
    return string.translate(ccNumString, allChars, badchars)

def parseCreditCard(s):
    """Parse the given credit card and return the appropriate object.

    >>> cc=parseCreditCard("4111111111111111")
    >>> isinstance(cc, Visa)
    True
    >>> cc.getMangledNum()
    'XXXXXXXXXXXX1111'
    >>> parseCreditCard("411111111111111")
    Traceback (most recent call last):
        [stack]
    InvalidCard: Invalid card length 15 expected one of 16, 13
    >>> parseCreditCard("4111111111111112")
    Traceback (most recent call last):
        [stack]
    InvalidCard: Checksum failed
    >>> isinstance(parseCreditCard('4222222222222'), Visa)
    True
    >>> parseCreditCard("422222222222")
    Traceback (most recent call last):
        [stack]
    InvalidCard: Invalid card length 12 expected one of 16, 13
    >>> isinstance(parseCreditCard('5105105105105100'), MasterCard)
    True
    >>> parseCreditCard("510510510510510")
    Traceback (most recent call last):
        [stack]
    InvalidCard: Invalid card length 15 expected one of 16, 13
    >>> parseCreditCard("5105105105105101")
    Traceback (most recent call last):
        [stack]
    InvalidCard: Checksum failed
    >>> isinstance(parseCreditCard('5-4-3-1-1111 1111 1111'), MasterCard)
    True
    >>> isinstance(parseCreditCard('378282246310005'), Amex)
    True
    >>> parseCreditCard("378282246310004")
    Traceback (most recent call last):
        [stack]
    InvalidCard: Checksum failed
    >>> parseCreditCard('37828224631000')
    Traceback (most recent call last):
        [stack]
    InvalidCard: Invalid card length 14 expected one of 15
    >>> isinstance(parseCreditCard('38520000023237'), Diner)
    True
    >>> parseCreditCard("38520000023238")
    Traceback (most recent call last):
        [stack]
    InvalidCard: Checksum failed
    >>> parseCreditCard('3852000002323')
    Traceback (most recent call last):
        [stack]
    InvalidCard: Invalid card length 13 expected one of 14
    >>> isinstance(parseCreditCard('6011111111111117'), Discover)
    True
    >>> parseCreditCard("6011111111111118")
    Traceback (most recent call last):
        [stack]
    InvalidCard: Checksum failed
    >>> parseCreditCard('601111111111111')
    Traceback (most recent call last):
        [stack]
    InvalidCard: Invalid card length 15 expected one of 16
    """
    theNum=normalize(s)
    ccClass=None

    prefixKeys=PREFIXES.keys()
    prefixKeys.sort(lambda a, b: cmp(len(a), len(b)))

    for prefix in prefixKeys:
        if theNum.find(prefix) == 0:
            ccClass=PREFIXES[prefix]

    if ccClass is None:
        raise InvalidCard("Unknown card prefix")

    return ccClass(theNum)

if __name__ == '__main__':
    doctest.testmod()
