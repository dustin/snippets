#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import re
import sys
import time

from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import letter
from reportlab.lib.units import inch

from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, Image
from reportlab.lib.styles import ParagraphStyle
from reportlab.rl_config import defaultPageSize
from reportlab.lib.enums import TA_CENTER
from reportlab.lib.units import inch

PAGE_WIDTH, PAGE_HEIGHT=letter

DATEFMT="%B %d, %Y"

def comma_me(amount):
    orig = str(amount)
    new = re.sub("^(-?\d+)(\d{3})", '\g<1>,\g<2>', str(amount))
    if orig == new:
        return new
    else:
        return comma_me(new)

def getStyles():
    return {
        'main': ParagraphStyle('main', spaceAfter=0.25 * inch, fontSize=14,
        leading=16),
        'plain': ParagraphStyle('main', fontSize=14, leading=16),
        'footer': ParagraphStyle('Footer', fontSize=8, leading=10,
        alignment=TA_CENTER)}

def go(parts):
    doc=SimpleDocTemplate("templated.pdf",
        leftMargin=1.5*inch, topMargin=1.75*inch)

    styles=getStyles()

    stuff=[]
    stuff.append(Paragraph(parts['date'], styles['plain']))
    stuff.append(Spacer(1, 0.25*inch))
    stuff.append(Paragraph(parts['name'], styles['plain']))
    stuff.append(Paragraph(parts['address'], styles['plain']))
    stuff.append(Paragraph("%(city)s, %(state)s  %(zip)s" % parts,
        styles['main']))

    stuff.append(Paragraph("Dear %(name)s:" % parts, styles['main']))

    stuff.append(Paragraph("""
Thank you for your recent donation of $%(amount)s to the Santa Clara Schools
Foundation. Your generous contribution is an investment in the future of our
community's children and ultimately everyone's future.
""" % parts, styles['main']))

    stuff.append(Paragraph("""
Since 1992 the foundation has been providing grants to individual classroom
teachers to help them enrich the learning experiences of their students and
that grant program will continue as before. The annual campaign to which you
have so generously responded will expand the foundation's efforts beyond
individual classrooms and enable us to help students on a district-wide basis.
As you obviously are aware, this is a critical need in these times of crippling
cuts to education funding.
""" % parts, styles['main']))

    stuff.append(Paragraph("""
Thank you again for your gift to the Santa Clara Schools Foundation. Your
contribution is genuinely appreciated and will be put to good use in improving
the education of our young people.
""" % parts, styles['main']))

    stuff.append(Paragraph("Sincerely,", styles['plain']))
    # Space before sig
    stuff.append(Spacer(1, 0.125*inch))

    # Signature here
    img=Image("paulfrench.jpg", width=2.5 * inch, height=0.40742 * inch)
    img.hAlign = 'LEFT'
    stuff.append(img)

    # space after sig
    # stuff.append(Spacer(1, 0.125*inch))

    stuff.append(Paragraph("Paul A. French", styles['plain']))
    stuff.append(Paragraph("President, Santa Clara Schools Foundation",
        styles['plain']))

    stuff.append(Spacer(1, 0.5*inch))

    stuff.append(Paragraph("""
Your contribution is fully tax deductible. No goods or services were provided
by the Santa Clara Schools Foundation in consideration for this contribution.
""" % parts, styles['footer']))

    doc.build(stuff)

if __name__ == '__main__':
    go({ 'name': "Dustin Sallings", 'date':time.strftime(DATEFMT),
        'address': '2589 Borax Dr.',
        'city': 'Santa Clara', 'state': 'CA', 'zip': '95051',
        'amount': comma_me(8528522523.25)})
