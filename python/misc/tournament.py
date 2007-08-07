#!/usr/bin/env python
"""
Tournament scoring stuff from an interview.

This program represents the results of a single-elimination tournament among 64
teams as a single 64-bit bitmap and allows expected results to be compared
against actual results.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import sys
import sets
import string
import random

NUM_TEAMS=64
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
assert len(ALL_TEAMS) == NUM_TEAMS, "Failed to build distinct teams"

def bit(num, pos):
    """True if the bit at position pos in the number num is 1."""
    return (num & ( 1 << pos ) != 0)

def splice(num, pos, size, numsize=NUM_TEAMS):
    """Get size bits of a number at position pos."""
    mask = 1
    for i in range(size-1):
        mask = 1 | mask << 1
    mask = mask << (numsize - pos - size)
    return (num & mask) >> (numsize - pos - size)

def score(exp, actual, pos, size, score_val, failed):
    """Compute the score for a single iteration."""

    ev=splice(exp, pos, size)
    av=splice(actual, pos, size)
    s=0
    ofailed=sets.Set()

    for i in range(size):
        # Skip over anything the previous test marked as failed.
        if i not in failed:
            if bit(ev, i) == bit(av, i):
                s += score_val
            else:
                ofailed.add(i/2)

    # Mark everything upstream as failed by halving the offset.
    for f in failed:
        ofailed.add(f / 2)

    return s, ofailed

def calc_score(exp, actual):
    """Calculate the score for someone based on the expected scores vs. the
    actual scores."""

    rv=0
    score_val=1
    size=32
    pos=0
    failed=sets.Set()

    while size > 0:
        s, failed=score(exp, actual, pos, size, score_val, failed)
        rv += s

        # Set up stuff for the next sequence
        pos += size
        size /= 2
        # Insert smarter business logic for calculating next score here.
        score_val *= 2

    return rv

def combine_teams(teams):
    """Combine a sequence of teams into pairs for a playoff."""
    assert len(teams) % 2 == 0, "Odd number of teams: " + `teams`
    # Convert to a list so we can do subsequences
    tl=sorted(list(teams))
    pairs=zip(tl[:len(tl)/2], tl[len(tl)/2:])
    assert len(pairs) == 1 or len(pairs[0]) == len(pairs[1])
    return tuple(pairs)

def encode_winnings(winnings):
    """Encode a sequence of sequence pairs into an integer."""
    rv=0
    def idx(p, el):
        if p[0] == el:
            return 0
        elif p[1] == el:
            return 1
        else:
            assert False, "Can't find " + el
    for series in winnings:
        for w in series:
            rv = (rv << 1) | (idx(w[0], w[1]))
    return rv << 1

def decode_wins(teams, wins):
    """Decode the wins integer into a sequence."""

    rv=[]

    current_teams = teams

    pos=0
    indent=0
    while current_teams:
        current=[]
        current_wins=splice(wins, pos, len(current_teams))
        winners=[]
        size=len(current_teams)
        for i in range(len(current_teams)):
            winner=current_teams[i][0]
            if bit(current_wins, size-1-i):
                winner=current_teams[i][1]
            current.append( (current_teams[i], winner) )
            winners.append(winner)
        rv.append(current)
        pos += size
        indent += 4
        if len(winners) > 1:
            current_teams = combine_teams(winners)
        else:
            current_teams = []

    return rv

def print_wins(teams, wins, fd=sys.stdout):
    indentAmt=0
    decoded=decode_wins(teams, wins)
    for series in decoded:
        indent=""
        if indentAmt > 0:
            indent = (" " * indentAmt) + "\\-> "
        for pair, winner in series:
            fd.write(indent + str(pair) + " -> " + str(winner) + "\n")
        indentAmt += 4
    fd.write("Final winner:  " + decoded[-1][0][1] + "\n")
