#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: skeleton.py,v 1.2 2003/01/03 08:43:26 dustin Exp $
"""

import sys
import string
import com

v5base='5225-2374-WHE2-22AZ-B2LW'

if __name__ == '__main__':
    baseKey=com.twowire.activation.Keycode.parseKeycode(v5base)

    f=open(sys.argv[1])
    for l in f.readlines():
        l=l.rstrip()
        a=string.split(l, maxsplit=3)
        mo=int(a[0])
        kc=a[1]
        other=a[-1]

        print "# " + other + " (mo=" + str(mo) + ")"
        baseKey.setOrgId(mo)
        print kc + "\t" + baseKey.toKeycodeString()
