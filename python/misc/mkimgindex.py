#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: FBC95722-4829-11D9-9A8D-000A957659CC

import os

def endswith(s, a):
    rv = False
    if len(s) > 4 and s[-4:].lower() in a:
        rv = True
    return rv

imgs=filter(lambda x: endswith(x, [".jpg", ".gif", ".png"]), os.listdir("."))
imgs.sort()

print """
<html><head><title>Image Index</title></head>
<ul>
"""

for img in imgs:
    print '<li><a href="%s">%s</a></li>' % (img, img)

print """
</ul>
</body>
</html>
"""
