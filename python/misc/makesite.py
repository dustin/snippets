#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: makesite.py,v 1.1 2003/09/12 07:53:10 dustin Exp $
"""

import os

def extractYears(dates):
    rvs={}
    for d in dates:
        rvs[d[0:4]]=1
    l=rvs.keys()
    l.sort()
    return(l)

def extractMonths(dates):
    rvs={}
    for d in dates:
        rvs[d[0:6]]=1
    l=rvs.keys()
    l.sort()
    return(l)

def getMonthsForYear(year, months):
    rv=[]
    for i in months:
        if i.startswith(year):
            rv.append(i)
    return(rv)

def getIDs(month, dates):
    dirs=[]
    for i in dates:
        if i.startswith(month):
            dirs.append(i)
    images=[]
    for d in dirs:
        thedir="images/" + d;
        files=os.listdir(thedir)
        for f in files:
            if f.find(".txt") > 0 and f.find("_d") > 0:
                offset=f.find("_d")
                images.append(d + "/" + f[0:offset])
    return(images)

def makeIndex(years):
    f=file("index.html", "w")
    f.write("""<html><head><title>Photos</title>
        <body bgcolor="#fFfFfF">
        Years:
        <ul>\n""")
    for y in years:
        f.write("<li><a href=\"" + y + ".html\">" + y + "</a></li>\n")
    f.write("</ul></body></html>\n")

def makeYearPage(year, months):
    f=file(year + ".html", "w")
    f.write("""<html><head><title>Photos from """ + year + """</title>
        <body bgcolor="#fFfFfF">
        Months:
        <ul>\n""")
    for m in months:
        f.write("<li><a href=\"" + m + ".html\">" + m[4:] + "</a></li>\n")
    f.write("</ul></body></html>\n")

def makeMonthPage(month, ids):
    f=file(month + ".html", "w")
    y=month[0:4]
    m=month[4:]
    f.write("""<html><head><title>Photos from """ + m + " " + y + """</title>
        <body bgcolor="#fFfFfF">\n""")
    for i in ids:
        theido=i.rindex("/")
        theid=i[theido+1:]
        f.write("<a href=\"" + month + "/" + theid + ".html\">"
            + "<img border=\"0\" src=\"images/" + i + "_t.jpg\"" + "></a>\n")
    f.write("</ul></body></html>\n")

def makePageForId(month, id):
    theido=id.rindex("/")
    theid=id[theido+1:]
    f=file(month + "/" + theid + ".html", "w")
    f.write("""<html><head><title>Image """ + theid + """</title>
        <body bgcolor="#fFfFfF">\n""")
    f.write("""<div align="center">\n""")
    f.write("<img src=\"../images/" + id + ".jpg\">\n")

    takents=id[0:id.index("/")]
    taken=takents[0:4] + "/" + takents[4:6] + "/" + takents[6:]

    f.write("<p>Taken: " + taken + "</p>\n")

    f.write("<p>Keywords: ")
    kf=file("images/" + id + "_k.txt")
    for l in kf.readlines():
        f.write(l)
    kf.close()
    f.write("</p>\n")

    f.write("<p>Description: ")
    kf=file("images/" + id + "_d.txt")
    for l in kf.readlines():
        f.write(l)
    kf.close()
    f.write("</p>\n")

    f.write("""</div>\n""")
    f.write("</body></html>\n")

dates=os.listdir("images")
years=extractYears(dates)
months=extractMonths(dates)

makeIndex(years)

for y in years:
    mfy=getMonthsForYear(y, months)
    print y + " contains " + `mfy`
    makeYearPage(y, mfy)

for m in months:
    print m + " ------"
    ids=getIDs(m, dates)
    makeMonthPage(m, ids)
    os.mkdir(m)
    for id in ids:
        makePageForId(m, id)
