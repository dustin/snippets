#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""

import os

try:
    True
except NameError:
	True, False = 1, 0

imageTypes=[".jpg", ".gif", ".png"]

def isImage(s):
    return len(s) > 4 and s[-4:].lower() in imageTypes

imgs=[fn for fn in os.listdir(".") if isImage(fn)]
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
