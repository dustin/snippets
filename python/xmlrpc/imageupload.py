#!/usr/bin/env python
#
# $Id: imageupload.py,v 1.2 2002/06/26 17:21:18 dustin Exp $

from sys import argv
import xmlrpclib
import time

class ImageUpload:

    def __init__(self, url='http://bleu.west.spy.net/photo/RPC2'):
        self.url=url

    def addImage(self, username=None, password=None, keywords=None,
        info=None, category=None, taken=None, image=None):

        server=xmlrpclib.Server(self.url)
        rv=server.addImage.addImage({
            'username':username,
            'password':password,
            'keywords':keywords,
            'info':info,
            'category':category,
            'taken':xmlrpclib.DateTime(taken),
            'image':xmlrpclib.Binary(image)
            });

        return(rv)


if __name__ == '__main__':
    if len(argv) < 8:
        theroof="Usage:  " + argv[0] + " url username password keywords info " \
            + "category taken filename ... "
        raise theroof
    url=argv[1]
    username=argv[2]
    password=argv[3]
    keywords=argv[4]
    info=argv[5]
    category=argv[6]
    taken=argv[7]

    # Rest of the arguments are images
    for filename in argv[8:]:

        f=open(filename)
        imageData=f.read()
        f.close()

        print "Image data is " + str(len(imageData)) + " bytes"

        uploader=ImageUpload(url)
        rv=uploader.addImage(username, password, keywords, info, category,
            taken, imageData)

        print "Added image " + `rv`
