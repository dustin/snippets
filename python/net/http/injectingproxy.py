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

class AccessDenied(exceptions.Exception):
    """Exception raised when an unauthorized URL is requested."""

class Injector:
    """Make the decision of when and what to inject."""

    acceptableMimeTypes=('text/html')
    blacklist=('www.yahoo.com',)

    def __init__(self, content="<h1>I was injected!!!</h1>"):
        self.content=content

    def shouldInject(self, proxyReq, mimeType):
        """Return true if the request should be injected."""
        (scm, netloc, path, params, query, fragment) = urlparse.urlparse(
            proxyReq.path, 'http')
        return mimeType in self.acceptableMimeTypes \
            and netloc not in self.blacklist

    def getInjection(self):
        """Return the data to inject."""
        return self.content

class ProxyHandler(BaseHTTPServer.BaseHTTPRequestHandler):
    """Handles each request."""

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

    def __resolveUrl(self, u):
        rv=u
        if rv[:4] != 'http':
            rv="http://localhost:80" + u
        try:
            (scm, netloc, path, params, query, fragment) = urlparse.urlparse(
                rv, 'http')
            bannedDomains=('.2wire.com',)
            bannedIps=('10.',)
            for b in bannedIps:
                if netloc[:len(b)] == b:
                    raise AccessDenied, rv
            for b in bannedDomains:
                if netloc[-len(b):] == b:
                    raise AccessDenied, rv
        except AccessDenied, e:
            logging.warn("Access denied: " + e[0])
            self.send_response(403)
            self.send_header("Content-Type", "text/plain")
            self.send_header("Connection", "close")
            self.send_header("Proxy-Connection", "close")
            self.end_headers()
            self.connection.send("Access denied: " + e[0])
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

if __name__ == '__main__':
    hdlr=logging.StreamHandler()
    fmt=logging.Formatter("%(levelname)s:%(asctime)s:%(message)s")
    hdlr.setFormatter(fmt)
    logging.root.addHandler(hdlr)
    logging.root.setLevel(logging.INFO)

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
    logging.info("Serving HTTP on %s:%d" % (sa[0], sa[1]))
    httpd.serve_forever()
