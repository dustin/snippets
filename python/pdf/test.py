#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: E57F021F-CEB2-4F96-AEA4-B34ACBB27519

from reportlab.pdfgen import canvas

def hello(c):
    c.drawString(100, 100, "Hello")

c = canvas.Canvas("hello.pdf")
hello(c)
c.showPage()
c.save()
