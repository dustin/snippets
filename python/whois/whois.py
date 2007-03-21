#!/usr/bin/env python

import socket
import string
import pprint

# List with multiple matches
class Format1:
    def __init__(self, data):
        self.tooBroad=None
        if data[0].find("Aborting search") >= 0:
            self.tooBroad=1
        self.todo=list()
        for i in range(len(data)):
            try:
                t=data[i]
                tmp=t[t.index('(')+1:t.rindex(')')]
                self.todo.append(tmp)
            except ValueError, ve:
                # If there isn't a paren set, we don't add it
                pass

    def __getitem__(self, which):
        return self.todo[which]

    def __len__(self):
        return len(self.todo)

    def getTodoList(self):
        return self.todo

    def wasTooBroad(self):
        return self.tooBroad

    def __str__(self):
        return str(self.todo)

# Document describing a single net block
class Format2:
    def __init__(self, data):
        self.longname=data[0]
        ln=self.longname
        self.shortname=ln[0:ln.index('(')].strip()
        self.netname=ln[ln.index('(')+1:ln.rindex(')')]
        self.addr=data[1].strip()
        csz=data[2].strip()
        self.city=csz[0:csz.index(',')]
        rest=csz[csz.index(',')+1:] 
        self.state, self.zip=rest.split()
        self.country=data[3].strip()

        self.fields=dict()
        for i in range(len(data)):
            a=data[i].split(': ', 2)
            if len(a) == 2:
                print "Split to", a
                self.fields[a[0].strip()]=a[1].strip()
        print "Fields: ", self.fields

    def getAddress(self):
        return self.addr, self.city, self.state, self.zip

    def __str__(self):
        return "A netblock named " + self.netname + " from " \
            + self.city + ", " + self.state

class Whois:
    def __init__(self, server = 'whois.arin.net'):
        self.server=server

    def lookup(self, spec):
        for res in socket.getaddrinfo(self.server, 43, 0, socket.SOCK_STREAM):
            af, socktype, proto, canonname, sa = res
            try:
                sock=socket.socket(af, socktype, proto)
                sock.connect(sa)
            except socket.error, msg:
                if sock:
                    sock.close()
                sock = None
                # Try the next address
                continue
            # If we got here, we have a socket
            break
        if not sock:
            raise socket.error, msg
        sendptr=0
        str=spec + '\r\n'
        while sendptr < len(str):
            sendptr = sendptr + sock.send(str[sendptr:])
        f=sock.makefile('r')
        rv=self.getData(f)
        f.close()
        sock.close()
        return rv

    def getData(self, f):
        data=self.readFile(f)
        rv=None
        if data[1][0:3] == '   ':
            rv=Format2(data)
        else:
            rv=Format1(data)
        return rv

    def readFile(self,f):
        rv=list()
        line=f.readline();
        while line!='':
            rv.append(line.rstrip())
            line=f.readline()
        return rv

    def getIPAddress(self, t):
        rv=()
        r=self.lookup(t)
        if isinstance(r, Format1):
            if r.wasTooBroad():
                print "***"
                print "The search was too broad, results may not be accurate"
                print "***"
            rv=self.lookup(r[-1]).getAddress()
        elif isinstance(r, Format2):
            print r
            rv=r.getAddress()
        else:
            raise "Unknown result format." + r.__name__
        return rv

def main():
    from sys import argv
    w=Whois()
    print w.getIPAddress(argv[1])

if __name__ == '__main__':
    main()
