#!/usr/bin/env python
"""
Convert hex encoded sha 1 hashes for torrents to Azureus magnet URLs.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: E8911DE2-EADE-4B75-AE2A-3825422CB7C0

import sys
import sha
import base64
import struct
import unittest

# Magnet URL form
# Family guy 4.5
# magnet:?xt=urn:btih:ZWFN4PTZBQPJPSYZOS754NLL4SNMR4OY
# As I find it on the site
# cd8ade3e790c1e97cb1974bfde356be49ac8f1d8

# Base32 (from python 2.4)  {{{
EMPTYSTRING = ''

_b32alphabet = {
    0: 'A',  9: 'J', 18: 'S', 27: '3',
    1: 'B', 10: 'K', 19: 'T', 28: '4',
    2: 'C', 11: 'L', 20: 'U', 29: '5',
    3: 'D', 12: 'M', 21: 'V', 30: '6',
    4: 'E', 13: 'N', 22: 'W', 31: '7',
    5: 'F', 14: 'O', 23: 'X',
    6: 'G', 15: 'P', 24: 'Y',
    7: 'H', 16: 'Q', 25: 'Z',
    8: 'I', 17: 'R', 26: '2',
    }

_b32tab = [v for v in _b32alphabet.values()]
_b32rev = dict([(v, long(k)) for k, v in _b32alphabet.items()])


def b32encode(s):
    """Encode a string using Base32.

    s is the string to encode.  The encoded string is returned.
    """
    parts = []
    quanta, leftover = divmod(len(s), 5)
    # Pad the last quantum with zero bits if necessary
    if leftover:
        s += ('\0' * (5 - leftover))
        quanta += 1
    for i in range(quanta):
        # c1 and c2 are 16 bits wide, c3 is 8 bits wide.  The intent of this
        # code is to process the 40 bits in units of 5 bits.  So we take the 1
        # leftover bit of c1 and tack it onto c2.  Then we take the 2 leftover
        # bits of c2 and tack them onto c3.  The shifts and masks are intended
        # to give us values of exactly 5 bits in width.
        c1, c2, c3 = struct.unpack('!HHB', s[i*5:(i+1)*5])
        c2 += (c1 & 1) << 16 # 17 bits wide
        c3 += (c2 & 3) << 8  # 10 bits wide
        parts.extend([_b32tab[c1 >> 11],         # bits 1 - 5
                      _b32tab[(c1 >> 6) & 0x1f], # bits 6 - 10
                      _b32tab[(c1 >> 1) & 0x1f], # bits 11 - 15
                      _b32tab[c2 >> 12],         # bits 16 - 20 (1 - 5)
                      _b32tab[(c2 >> 7) & 0x1f], # bits 21 - 25 (6 - 10)
                      _b32tab[(c2 >> 2) & 0x1f], # bits 26 - 30 (11 - 15)
                      _b32tab[c3 >> 5],          # bits 31 - 35 (1 - 5)
                      _b32tab[c3 & 0x1f],        # bits 36 - 40 (1 - 5)
                      ])
    encoded = EMPTYSTRING.join(parts)
    # Adjust for any leftover partial quanta
    if leftover == 1:
        return encoded[:-6] + '======'
    elif leftover == 2:
        return encoded[:-4] + '===='
    elif leftover == 3:
        return encoded[:-3] + '==='
    elif leftover == 4:
        return encoded[:-1] + '='
    return encoded


def b32decode(s, casefold=False, map01=None):
    """Decode a Base32 encoded string.

    s is the string to decode.  Optional casefold is a flag specifying whether
    a lowercase alphabet is acceptable as input.  For security purposes, the
    default is False.

    RFC 3548 allows for optional mapping of the digit 0 (zero) to the letter O
    (oh), and for optional mapping of the digit 1 (one) to either the letter I
    (eye) or letter L (el).  The optional argument map01 when not None,
    specifies which letter the digit 1 should be mapped to (when map01 is not
    None, the digit 0 is always mapped to the letter O).  For security
    purposes the default is None, so that 0 and 1 are not allowed in the
    input.

    The decoded string is returned.  A TypeError is raised if s were
    incorrectly padded or if there are non-alphabet characters present in the
    string.
    """
    quanta, leftover = divmod(len(s), 8)
    if leftover:
        raise TypeError('Incorrect padding')
    # Handle section 2.4 zero and one mapping.  The flag map01 will be either
    # False, or the character to map the digit 1 (one) to.  It should be
    # either L (el) or I (eye).
    if map01:
        s = _translate(s, {'0': 'O', '1': map01})
    if casefold:
        s = s.upper()
    # Strip off pad characters from the right.  We need to count the pad
    # characters because this will tell us how many null bytes to remove from
    # the end of the decoded string.
    padchars = 0
    mo = re.search('(?P<pad>[=]*)$', s)
    if mo:
        padchars = len(mo.group('pad'))
        if padchars > 0:
            s = s[:-padchars]
    # Now decode the full quanta
    parts = []
    acc = 0
    shift = 35
    for c in s:
        val = _b32rev.get(c)
        if val is None:
            raise TypeError('Non-base32 digit found')
        acc += _b32rev[c] << shift
        shift -= 5
        if shift < 0:
            parts.append(binascii.unhexlify(hex(acc)[2:-1]))
            acc = 0
            shift = 35
    # Process the last, partial quanta
    last = binascii.unhexlify(hex(acc)[2:-1])
    if padchars == 1:
        last = last[:-1]
    elif padchars == 3:
        last = last[:-2]
    elif padchars == 4:
        last = last[:-3]
    elif padchars == 6:
        last = last[:-4]
    elif padchars <> 0:
        raise TypeError('Incorrect padding')
    parts.append(last)
    return EMPTYSTRING.join(parts)
# }}}

def unhex(s):
    """Unhexify a hex string."""
    # Must be an even number of digits
    assert len(s) % 2 == 0
    rv=""
    for i in range(len(s)/2):
        pair=s[i*2:(i*2)+2]
        rv+=str(chr(int(pair, 16)))
    return rv

def hexToBase64(s):
    return base64.encodestring(unhex(s)).strip()

def hexToBase32(s):
    return b32encode(unhex(s)).strip()

class Tests(unittest.TestCase):

    def testUnhex(self):
        self.assertEquals(unhex("44757374696e"), "Dustin")

    def testHexToBase64(self):
        self.assertEquals(hexToBase64("44757374696e"), "RHVzdGlu")

    def testFamilyGuy(self):
        self.assertEquals(
            hexToBase32("cd8ade3e790c1e97cb1974bfde356be49ac8f1d8"),
            "ZWFN4PTZBQPJPSYZOS754NLL4SNMR4OY")

if __name__ == '__main__':
    for s in sys.argv[1:]:
        print "magnet:?xt=urn:btih:%s" % hexToBase32(s)

# vim: foldmethod=marker
