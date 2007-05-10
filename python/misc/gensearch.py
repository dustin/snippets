#!/usr/bin/env python
"""
Generate a C function to match strings.

Input: a tab separated list of k->v pairs.

Copyright (c) 2006  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 380AB186-BD0D-4302-837B-A45CD42B699E

import sys
import sets

def buildIt(depth, prefix, stuff, fanout_below, caseSensitive):
    # Find all of the stuff that matches this prefix exactly.
    tab=(1+depth*2) * "\t"
    matches=[(n[len(prefix):],c) for (n,c) in stuff if n.startswith(prefix)]
    if len(matches) < fanout_below:
        first=True
        print tab + "/* " + str(len(matches)) + ' match(es) for "' \
            + prefix + '" */'
        for m in matches:
            w=prefix + m[0]
            if not caseSensitive:
                w=w.upper()
            l=len(w)
            p=" else "
            if first:
                p = tab
                first=False
            print p + 'if(memcmp(p, "' + w + '\\0", ' \
                + str(l+1) + ') == 0) {'
            print tab + '\trv=' + m[1] + ';'
            sys.stdout.write(tab + '}')
        print ""
    else:
        prefixes=sets.Set([n[0][0] for (n,c) in matches])
        print tab + "/* " + str(len(matches)) + ' matches for "' \
            + prefix + '" */'
        print tab + 'switch(p[' + str(depth) + ']) {'
        for p in prefixes:
            print tab + "\tcase '" + p + "':"
            buildIt(depth+1, prefix + p, stuff, fanout_below, caseSensitive)
            print tab + "\tbreak;"
        print tab + '}'

def genCaseInsensitiveMatch(fName, stuff, fanout_below):
    minlen=min([len(a) for (a,b) in stuff])
    maxlen=max([len(a) for (a,b) in stuff])

    print """#include <string.h>
#include <ctype.h>
#include <assert.h>

int %(fn)s(const char *input) {
    int rv=-1;
    char uppercased[%(maxlen)d+1]; /* Maximum length of acceptable input */
    const char *p=input;
    int i=0;
    int len=strlen(input);

    /* Short cut when the length falls outside of expected ranges */
    if(len < %(minlen)d || len > %(maxlen)d) {
        return rv;
    }

    /* Create an uppercase form of our input so the matching is sure to work */
    while(*p) {
        uppercased[i++]=toupper(*p++);
    }
    uppercased[i]=0x00;

    /* Walk over the uppercased version for a case-insensitive search. */
    p=uppercased;

    /* Collapsing anything with fewer than %(fanout)d paths */""" \
    % {'fn': fName, 'fanout': fanout_below, 'maxlen': maxlen,
        'minlen': minlen}

    buildIt(0, "", stuff, fanout_below, False)

    print "\treturn rv;\n}"

def genCaseSensitiveMatch(fName, stuff, fanout_below):
    minlen=min([len(a) for (a,b) in stuff])
    maxlen=max([len(a) for (a,b) in stuff])

    print """#include <string.h>
#include <ctype.h>
#include <assert.h>

int %(fn)s(const char *input) {
    int rv=-1;
    const char *p=input;
    int len=strlen(input);

    /* Short cut when the length falls outside of expected ranges */
    if(len < %(minlen)d || len > %(maxlen)d) {
        return rv;
    }

    /* Collapsing anything with fewer than %(fanout)d paths */""" \
    % {'fn': fName, 'fanout': fanout_below, 'maxlen': maxlen,
        'minlen': minlen}

    buildIt(0, "", stuff, fanout_below, True)

    print "\treturn rv;\n}"

TYPES={'cs': genCaseSensitiveMatch, 'ci': genCaseInsensitiveMatch}

def usage():
    sys.stderr.write("Usage:  %s functionName matchType [fanoutSize]\n" % 
        sys.argv[0])
    sys.stderr.write("""Available match types:
    cs  generate a case sensitive match.
    ci  generate a case insensitive match.\n""")
    sys.exit(1)

if __name__ == '__main__':

    if len(sys.argv) < 3:
        usage()

    fName, searchType = sys.argv[1:3]

    if searchType not in TYPES:
        usage()

    fanout_below=4
    if len(sys.argv) > 3:
        fanout_below=int(sys.argv[3])

    stuff=[l.strip().split() for l in sys.stdin.readlines()]

    TYPES[searchType](sys.argv[1], stuff, fanout_below)
