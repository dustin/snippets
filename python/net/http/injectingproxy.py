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
import logging
import traceback
import getopt
import random
import sets
import re

class AccessDenied(exceptions.Exception):
    """Exception raised when an unauthorized URL is requested."""

class Injector:
    """Make the decision of when and what to inject."""

    def __init__(self, files, blacklist=[], mimeTypes=('text/html',)):
        self.blacklist=blacklist
        self.acceptableMimeTypes=mimeTypes
        r=random.Random()
        def randomFile():
            return r.choice(files)
        self.randomFile=randomFile

    def shouldInject(self, proxyReq, mimeType):
        """Return true if the request should be injected."""
        (scm, netloc, path, params, query, fragment) = urlparse.urlparse(
            proxyReq.path, 'http')
        return mimeType in self.acceptableMimeTypes \
            and netloc != '' and netloc not in self.blacklist

    def getInjection(self):
        """Return the data to inject."""
        aFile=self.randomFile()
        f=open(aFile)
        try:
            rv=f.read()
        finally:
            f.close()
        return rv

class ProxyHandler(BaseHTTPServer.BaseHTTPRequestHandler):
    """Handles each request."""

    # An injector must be, um, injected into this class
    injector=None
    bannedPatterns=[]
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
                colon=h.index(":")
                k=h[:colon]
                v=h[colon+1:].strip()
                rv.append((k, v))
        for h in inject:
            rv.append(h)
        return rv

    def __getUsableOutHeaders(self, headers):
        return self.__mungeHeaders(headers, self.unusableOutHeaders,
            (('Connection', 'close'), ('Proxy-Connection', 'close'),
            ('X-Served-Via', self.server_version)))

    def __getUsableInHeaders(self, headers):
        return self.__mungeHeaders(headers, self.unusableInHeaders,
            (('Connection', 'close'),))

    # Disable default logging
    def log_request(self, code='-', size='-'):
        pass

    def __handleInjection(self, response, data):
        sender=self.connection.send
        close=self.connection.close
        # Lots of special gzip handling
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

    def __respond(self, req):
        code=200
        msg="OK"
        try:
            res=urllib2.urlopen(req)
        except urllib2.HTTPError, e:
            res=e
            code=e.code
            msg=e.msg
        headers=self.__getUsableOutHeaders(res.info())
        self.send_response(code)
        for h in headers:
            self.send_header(h[0], h[1])
        self.end_headers()
        if code == 200 and self.injector.shouldInject(self, res.info().type):
            logging.info("%s-%d-Injecting into %s" % (req.get_method(),
                code, self.path))
            self.__handleInjection(res, res.read())
        else:
            logging.info("%s-%d-Not injecting into %s" % (req.get_method(),
                code, self.path))
            self.connection.send(res.read())
        res.close()

    def __sendText(self, code, msg):
        self.send_response(code)
        self.send_header("Content-Type", "text/plain")
        self.send_header("Connection", "close")
        self.send_header("Proxy-Connection", "close")
        self.end_headers()
        self.connection.send(msg)

    def __resolveUrl(self, u):
        rv=u
        if rv[:4] != 'http':
            if self.defaultUrl is None:
                msg="No default URL, and not a proxy request"
                self.__sendText(400, msg)
                raise exceptions.Exception, msg
            else:
                rv=self.defaultUrl + u
                logging.info("Using default URL to get " + rv)
        try:
            (scm, netloc, path, params, query, fragment) = urlparse.urlparse(
                rv, 'http')
            for p in self.bannedPatterns:
                if p.search(netloc):
                    raise AccessDenied, rv
        except AccessDenied, e:
            self.__sendText(403, "Access denied: " + e[0])
            raise e
        return rv

    def do_GET(self):
        inHeaders=self.__getUsableInHeaders(self.headers)
        req=urllib2.Request(self.__resolveUrl(self.path), None, dict(inHeaders))
        self.__respond(req)

    def do_POST(self):
        inHeaders=self.__getUsableInHeaders(self.headers)
        toRead=int(self.headers['Content-length'])
        req=urllib2.Request(self.__resolveUrl(self.path),
            self.connection.recv(toRead), dict(inHeaders))
        self.__respond(req)

class ThreadingHTTPServer(SocketServer.ThreadingMixIn,
    BaseHTTPServer.HTTPServer):

    def handle_error(self, request, client_address):
        etype, value, tb = sys.exc_info()
        logging.warn("Error occurred from %s\n%s" % (client_address[0],
            "".join(traceback.format_exception(etype, value, tb))))

class UsageError(exceptions.Exception):
    """Exception thrown for an invalid usage."""

def main():
    port=8000
    noInjectList=sets.Set()
    denied=[]
    mimeTypes=sets.Set()
    mimeTypes.add("text/html")
    defaultUrl=None

    try:
        opts, args = getopt.getopt(sys.argv[1:], 'd:n:x:p:m:')
    except getopt.GetoptError, e:
        raise UsageError(e)

    for op, val in opts:
        if op == '-p':
            port=int(val)
        if op == '-d':
            defaultUrl=val.rstrip('/')
        if op == '-m':
            mimeTypes.add(val)
        if op == '-n':
            f=open(val)
            noInjectList=sets.Set([x.strip() for x in f.readlines()])
            f.close()
        if op == '-x':
            f=open(val)
            denied=[re.compile(x.strip()) for x in f.readlines() if x[0] != '#']
            f.close()

    if len(args) == 0:
        raise UsageError, "You need to provide at least one injection file."

    # Logger setup
    hdlr=logging.StreamHandler()
    fmt=logging.Formatter("%(levelname)s:%(asctime)s:%(message)s")
    hdlr.setFormatter(fmt)
    logging.root.addHandler(hdlr)
    logging.root.setLevel(logging.INFO)

    # Get the listening address
    server_address = ('', port)

    # Set up the injector if we get a filename.
    ProxyHandler.injector=Injector(args, noInjectList, mimeTypes)
    ProxyHandler.bannedPatterns=denied
    ProxyHandler.defaultUrl=defaultUrl

    # Get the server going
    httpd = ThreadingHTTPServer(server_address, ProxyHandler)

    sa = httpd.socket.getsockname()
    logging.info("Serving HTTP on %s:%d" % (sa[0], sa[1]))
    logging.info("Servicing the following types:  " + ', '.join(mimeTypes))
    logging.info("Denying access to %d sites, ignoring %d" \
        % (len(denied), len(noInjectList)))
    if defaultUrl is not None:
        logging.info("Default URL is " + defaultUrl)
    httpd.serve_forever()

if __name__ == '__main__':
    try:
        main()
    except UsageError, e:
        print e
        print "Usage:  " + sys.argv[0] + \
            " [-d defaultUrl] [-n file] [-m type [-m type ...]]"
        print "\t\t[-p port] [-x file] injectfile [injectfile ...]"
        print "\t-d is used to specify a default URL for non-proxy operation"
        print "\t-n a file containing hosts to ignore"
        print "\t-x a file containing hostname patterns that are denied"
        print "\t-p the port to listen on"
        print "\t-m adds a mime type to the acceptable mime type list"
