#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

from reportlab.pdfgen import canvas

def hello(c):
    c.drawString(100, 100, "Hello")

c = canvas.Canvas("hello.pdf")
hello(c)
c.showPage()
c.save()
