#!/usr/bin/env python
"""

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import sets
import random
import unittest
import StringIO

import tournament

class BitStuffTestCase(unittest.TestCase):

    def testBitPos(self):
        """Test the simple bit position stuff."""
        self.assertTrue(tournament.bit(5, 0))
        self.assertFalse(tournament.bit(5, 1))

    def testBitSpliceAtZero(self):
        """Validating pulling out some bits at position zero works."""
        self.assertEquals(tournament.splice(38384, 0, 4, 16), 9)

    def testBitSpliceAtFour(self):
        """Validating pulling out some bits at position four works."""
        self.assertEquals(tournament.splice(38384, 4, 4, 16), 5)

    def testBitSpliceAtZero8Bit(self):
        """Validating 8-bit style"""
        self.assertEquals(tournament.splice(38384, 0, 8, 8), 240)

    def test64Bit(self):
        """Test a 64-bit splice."""
        self.assertEquals(tournament.splice(1124935821487799207, 0, 32),
            261919531)

class ScoringTest(unittest.TestCase):

    # I generated a list of teams with random names.  Real teams would probably
    # be a bit easier to deal with.
    ALL_TEAMS=sets.ImmutableSet(['ehqk', 'bsmv', 'svhl', 'huyr', 'aevo', 'mknq',
        'nksx', 'ymxw', 'xebf', 'axci', 'unet', 'glqw', 'umav', 'nezy', 'unoj',
        'crkq', 'ksrt', 'csny', 'djkp', 'fpmd', 'ared', 'lgou', 'rxzp', 'jtmu',
        'topc', 'bdmo', 'ohcg', 'mzlx', 'dnur', 'zgty', 'pzxe', 'ucbp', 'wfyl',
        'ekji', 'slup', 'pmor', 'sklm', 'fpgd', 'xrob', 'xzgp', 'woxm', 'neha',
        'svew', 'umse', 'icrz', 'zvky', 'wyls', 'muhf', 'msdp', 'vzkr', 'anye',
        'cyuo', 'zgkq', 'sqdv', 'tdrm', 'nktw', 'kevf', 'iksc', 'lnzy', 'rbon',
        'xatw', 'fbva', 'glch', 'udob'])

    # An example randomly generated sequence.
    SEQS=[[(('aevo', 'nksx'), 'aevo'), (('anye', 'nktw'), 'anye'), # 0, 0   64
        (('ared', 'ohcg'), 'ared'), (('axci', 'pmor'), 'pmor'),    # 0, 1
        (('bdmo', 'pzxe'), 'pzxe'), (('bsmv', 'rbon'), 'rbon'),    # 1, 1
        (('crkq', 'rxzp'), 'rxzp'), (('csny', 'sklm'), 'sklm'),    # 1, 1
        (('cyuo', 'slup'), 'cyuo'), (('djkp', 'sqdv'), 'djkp'),    # 0, 0
        (('dnur', 'svew'), 'svew'), (('ehqk', 'svhl'), 'svhl'),    # 1, 1
        (('ekji', 'tdrm'), 'tdrm'), (('fbva', 'topc'), 'fbva'),    # 1, 0
        (('fpgd', 'ucbp'), 'fpgd'), (('fpmd', 'udob'), 'udob'),    # 0, 1
        (('glch', 'umav'), 'glch'), (('glqw', 'umse'), 'glqw'),    # 0, 0
        (('huyr', 'unet'), 'unet'), (('icrz', 'unoj'), 'icrz'),    # 1, 0
        (('iksc', 'vzkr'), 'iksc'), (('jtmu', 'wfyl'), 'wfyl'),    # 0, 1
        (('kevf', 'woxm'), 'woxm'), (('ksrt', 'wyls'), 'ksrt'),    # 1, 0
        (('lgou', 'xatw'), 'lgou'), (('lnzy', 'xebf'), 'xebf'),    # 0, 1
        (('mknq', 'xrob'), 'mknq'), (('msdp', 'xzgp'), 'xzgp'),    # 0, 1
        (('muhf', 'ymxw'), 'muhf'), (('mzlx', 'zgkq'), 'zgkq'),    # 0, 1
        (('neha', 'zgty'), 'zgty'), (('nezy', 'zvky'), 'nezy')],   # 1, 0   32
    [(('aevo', 'pmor'), 'pmor'), (('anye', 'pzxe'), 'pzxe'),       # 1, 1
        (('ared', 'rbon'), 'ared'), (('cyuo', 'rxzp'), 'cyuo'),    # 0, 0
        (('djkp', 'sklm'), 'djkp'), (('fbva', 'svew'), 'svew'),    # 0, 1
        (('fpgd', 'svhl'), 'fpgd'), (('glch', 'tdrm'), 'tdrm'),    # 0, 1
        (('glqw', 'udob'), 'udob'), (('icrz', 'unet'), 'unet'),    # 1, 1
        (('iksc', 'wfyl'), 'iksc'), (('ksrt', 'woxm'), 'ksrt'),    # 0, 0
        (('lgou', 'xebf'), 'xebf'), (('mknq', 'xzgp'), 'mknq'),    # 1, 0
        (('muhf', 'zgkq'), 'muhf'), (('nezy', 'zgty'), 'nezy')],   # 0, 0
    [(('ared', 'nezy'), 'nezy'), (('cyuo', 'pmor'), 'pmor'),       # 1, 1
        (('djkp', 'pzxe'), 'pzxe'), (('fpgd', 'svew'), 'svew'),    # 1, 1
        (('iksc', 'tdrm'), 'iksc'), (('ksrt', 'udob'), 'udob'),    # 0, 1
        (('mknq', 'unet'), 'unet'), (('muhf', 'xebf'), 'xebf')],   # 1, 1
    [(('iksc', 'svew'), 'iksc'), (('nezy', 'udob'), 'udob'),       # 0, 1
        (('pmor', 'unet'), 'pmor'), (('pzxe', 'xebf'), 'pzxe')],   # 0, 0
    [(('iksc', 'pzxe'), 'pzxe'), (('pmor', 'udob'), 'udob')],      # 1, 1
    [(('pzxe', 'udob'), 'udob')]]                                  # 1, 0   0

    # 0001111100111001001001100101011011000101110010001111011101001110
    ENCODED=2249871642975598414

    def testScoreSimple(self):
        """Simple scoring test with no input mask."""
        # Compute an offset near the end of the bit array since we're working
        # with small numbers.
        offset=64-8
        s, failed=tournament.score(15, 7, offset, 8, 1, [])
        self.assertEquals(s, 7) # Of eight bits, only one was different
        self.assertEquals(len(failed), 1)
        self.assertTrue(1 in failed)

    def testScoreFailureSkipping(self):
        """Scoring test skipping matches based on previous mask."""
        # Compute an offset near the end of the bit array since we're working
        # with small numbers.
        offset=64-8
        s, failed=tournament.score(15, 7, offset, 8, 1, [0, 1, 2])
        self.assertEquals(s, 4) # Scoring based on similarities minus exceptions
        self.assertEquals(len(failed), 2)
        # These were carried forward from the previous test
        self.assertTrue(0 in failed)
        self.assertTrue(1 in failed)

    def testCombining(self):
        """Test team combination."""
        pairs=tournament.combine_teams('abcdef')
        self.assertEquals((('a', 'd'), ('b', 'e'), ('c', 'f')), pairs)

    def testPrintingAllLeft(self):
        """Test the printing function all left."""
        s=StringIO.StringIO()
        tournament.print_wins(tournament.combine_teams('abcdefgh'), 0, s)
        s.seek(0)
        l=s.readlines()
        self.assertEquals("Final winner:  a\n", l[-1])

    def testPrintingAllRight(self):
        """Test the printing function all right"""
        s=StringIO.StringIO()
        tournament.print_wins(tournament.combine_teams('abcdefgh'), ~0, s)
        s.seek(0)
        l=s.readlines()
        self.assertEquals("Final winner:  h\n", l[-1])

    def testEncodingWinnings(self):
        """Test encoding winnings into an integer."""
        exp=self.ENCODED
        score=tournament.encode_winnings(self.SEQS)
        self.assertEquals(self.ENCODED, score)

    def testDecodingWinnings(self):
        """Test decoding winnings from a number."""
        decoded=tournament.decode_wins(
            tournament.combine_teams(self.ALL_TEAMS),
            self.ENCODED)
        self.assertEquals(self.SEQS, decoded)

    def testPrintingWinner(self):
        """Test printing a real winner calculation."""
        s=StringIO.StringIO()
        tournament.print_wins(tournament.combine_teams(self.ALL_TEAMS),
            self.ENCODED, s)
        s.seek(0)
        l=s.readlines()
        self.assertEquals("Final winner:  udob\n", l[-1])

    def testScoringPerfect(self):
        """Test scoring with a perfect score."""
        score=tournament.calc_score(self.ENCODED, self.ENCODED)
        # Perfect score == 32 * 6
        self.assertEquals(192, score)

    def testScoringImperfect(self):
        """Test a score that was perfect until the last game."""
        score=tournament.calc_score(self.ENCODED, self.ENCODED & (~3))
        # perfect - 32
        self.assertEquals(160, score)

    def testScoringCut(self):
        """Test a score that that is wrong at the very beginning."""
        score=tournament.calc_score(self.ENCODED, 1096950138368751438)
        # Missed one of the first ones.  Loses a point at each level.
        # perfect - 32 - 16 - 8 - 4 - 2 - 1
        self.assertEquals(129, score)

if __name__ == '__main__':
    unittest.main()
