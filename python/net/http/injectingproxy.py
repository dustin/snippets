#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: DFB2FDBB-8C55-42E1-B5CB-157A9B81335D

import sys
import BaseHTTPServer
import exceptions
import SocketServer
import urlparse
import urllib2
import gzip
import cStringIO

class StupidResponse:
    def __init__(self, u, h):
        self.u=u
        self.h=h
    def geturl(self):
        return self.u
    def info(self):
        return self.h

class ErrorHandler(urllib2.HTTPDefaultErrorHandler):
    def __init__(self, proxy):
        self.proxy=proxy

    def http_error_default(self, req, fp, code, msg, hdrs):
        s=self.proxy.connection
        print code, "for", req.get_full_url()
        s.send("HTTP/1.0 " + `code` + " " + msg + "\r\n")
        s.send(self.proxy.convertHeaders(self.proxy.getUsableOutHeaders(hdrs)))
        s.send(fp.read())
        s.close()
        return StupidResponse(req, hdrs)

class Injector:

    acceptableMimeTypes=('text/html')
    blacklist=('www.yahoo.com',)

    def __init__(self, content="<h1>I was injected!!!</h1>"):
        self.content=content

    def shouldInject(self, proxyReq, mimeType):
        (scm, netloc, path, params, query, fragment) = urlparse.urlparse(
            proxyReq.path, 'http')
        return mimeType in self.acceptableMimeTypes \
            and netloc not in self.blacklist

    def getInjection(self):
        return self.content

class ProxyHandler(BaseHTTPServer.BaseHTTPRequestHandler):

    injector = Injector()
    protocol_version="HTTP/1.0"
    server_version = "InjectingProxy/1.0"
    unusableOutHeaders=['connection', 'content-length']
    unusableInHeaders=['connection', 'keep-alive', 'proxy-connection', 'host']
    rbufsize = 0

    def __mungeHeaders(self, headers, unusable, inject):
        rv=[]
        for h in headers.headers:
            usable=True
            for u in unusable:
                if h.lower().find(u + ":") >= 0:
                    usable=False
            if usable:
                rv.append(h.strip())
        for h in inject:
            rv.append(h)
        return rv

    def getUsableOutHeaders(self, headers):
        return self.__mungeHeaders(headers, self.unusableOutHeaders,
            ('Connection: close', 'Proxy-Connection: close',
            'X-Served-Via: ' + self.server_version))

    def getUsableInHeaders(self, headers):
        return self.__mungeHeaders(headers, self.unusableInHeaders,
            ('Connection: close',))

    def headersToDict(self, headers):
        rv={}
        for h in headers:
            colon=h.index(":")
            k=h[:colon]
            v=h[colon+1:].strip()
            rv[k]=v
        return rv

    def convertHeaders(self, headers):
        return '\r\n'.join(headers) + "\r\n\r\n"

    def getOpener(self):
        return urllib2.build_opener(ErrorHandler(self))

    def handleInjection(self, response, data):
        sender=self.connection.send
        close=self.connection.close
        # Lots of special gzip  handling
        if response.info().getheader('Content-Encoding') == 'gzip':
            data=gzip.GzipFile('', 'r', 0, cStringIO.StringIO(data)).read()
            class Writer:
                def write(s, b):
                    self.connection.send(b)
            out=gzip.GzipFile('', 'w', 3, Writer())
            sender=out.write
            close=out.close

        bodyStart=data.lower().find("<body")

        if bodyStart >= 0:
            sender(data[:bodyStart])
            data=data[bodyStart:]
            pos=data.find(">")+1
            sender(data[:pos])
            sender(self.injector.getInjection())
            sender(data[pos:])
        else:
            sender(data)

        close()

    def do_GET(self):
        inHeaders=self.getUsableInHeaders(self.headers)
        print "GET", self.path
        req=urllib2.Request(self.path, None, self.headersToDict(inHeaders))
        response=self.getOpener().open(req)
        self.sendResponse(response)

    def do_POST(self):
        inHeaders=self.getUsableInHeaders(self.headers)
        print "POST", self.path
        toRead=int(self.headers['Content-length'])
        req=urllib2.Request(self.path, self.connection.recv(toRead),
            self.headersToDict(inHeaders))
        response=self.getOpener().open(req)
        self.sendResponse(response)

    def sendResponse(self, response):
        if not isinstance(response, StupidResponse):
            headers=self.getUsableOutHeaders(response.info())
            self.connection.send("HTTP/1.0 200 OK\r\n")
            self.connection.send(self.convertHeaders(headers))
            if self.injector.shouldInject(self, response.info().type):
                print "Injecting into", self.path
                self.handleInjection(response, response.read())
            else:
                print "Not injecting into", self.path
                self.connection.send(response.read())
            response.close()

class ThreadingHTTPServer(SocketServer.ThreadingMixIn,
    BaseHTTPServer.HTTPServer):
    pass

if __name__ == '__main__':
    # Get the listening address
    server_address = ('', 8000)

    # Set up the injector if we get a filename.
    if len(sys.argv) > 1:
        f=open(sys.argv[1])
        ProxyHandler.injector=Injector(f.read())
        f.close()

    # Get the server going
    httpd = ThreadingHTTPServer(server_address, ProxyHandler)

    sa = httpd.socket.getsockname()
    print "Serving HTTP on", sa[0], "port", sa[1], "..."
    httpd.serve_forever()
