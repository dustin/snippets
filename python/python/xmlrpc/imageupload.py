#!/usr/bin/env python
#
# $Id: imageupload.py,v 1.1 2002/06/25 01:18:42 dustin Exp $

from sys import argv
import xmlrpclib
import time

password=argv[1]
filename=argv[2]

f=open(filename)
imageData=f.read()
f.close()

print "Image data is " + str(len(imageData)) + " bytes"

server=xmlrpclib.Server('http://bleu.west.spy.net/photo/RPC2')
server.addImage.addImage({
	'username':'dustin',
	'password':password,
	'keywords':'test image',
	'info':'This is a test image.',
	'category':'DPrivate',
	'taken':xmlrpclib.DateTime(),
	'image':xmlrpclib.Binary(imageData)
	});
