#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import re
import time

from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import letter
from reportlab.lib.units import inch

DATEFMT="%B %d, %Y"

def comma_me(amount):
    orig = str(amount)
    new = re.sub("^(-?\d+)(\d{3})", '\g<1>,\g<2>', str(amount))
    if orig == new:
        return new
    else:
        return comma_me(new)

def hello(c):
    to=c.beginText(1.5 * inch, 9.25 * inch)
    to.textLines("""
%(date)s



%(name)s
%(address)s
%(city)s, %(state)s  %(zip)s

Dear %(name)s:

Thank you for your recent donation of $%(amount)s to the Santa Clara Schools
Foundation. Your generous contribution is an investment in the future of our
community's children and ultimately everyone's future.

Since 1992 the foundation has been providing grants to individual classroom
teachers to help them enrich the learning experiences of their students and
that grant program will continue as before. The annual campaign to which you
have so generously responded will expand the foundation's efforts beyond
individual classrooms and enable us to help students on a district-wide basis.
As you obviously are aware, this is a critical need in these times of crippling
cuts to education funding.

Thank you again for your gift to the Santa Clara Schools Foundation. Your
contribution is genuinely appreciated and will be put to good use in improving
the education of our young people.

Sincerely,




Paul A. French
President, Santa Clara Schools Foundation

""" % { 'name': "Dustin Sallings", 'date':time.strftime(DATEFMT),
    'address': '2589 Borax Dr.',
    'city': 'Santa Clara', 'state': 'CA', 'zip': '95051',
    'amount': comma_me(2523.25)})

    c.drawText(to)

    x, y=to.getCursor()
    footerto=c.beginText(1.5 * inch, y - (1 * inch))
    footerto.setFont("Helvetica", 10)
    footerto.textLines("""
Your contribution is fully tax deductible. No goods or services were provided
by the Santa Clara Schools Foundation in consideration for this contribution.
""")

    c.drawText(footerto)

c = canvas.Canvas("big.pdf", pagesize=letter, pageCompression=False)
hello(c)
c.showPage()
c.save()
