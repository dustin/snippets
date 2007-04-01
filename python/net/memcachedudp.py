#!/usr/bin/env python
"""
UDP-based memcached client.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import sys
import math
import random
import socket
import struct

FRAME_HEADER_FMT=">HHHH"

RCV_BUFFER_SIZE=16384

class MemcachedHost(object):

    msgSize=1024

    def __init__(self, host, port=11211):
        self.remote=host, port
        self.reqid=random.randint(0, 65535)
        self.socket=socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.socket.bind(('', 0))

    def __nextId(self):
        self.reqid += 1
        if self.reqid >= 65535:
            self.reqid=0
        return self.reqid

    def getMsg(self):
        data, fromaddr=self.socket.recvfrom(RCV_BUFFER_SIZE)
        (id, seq, total, zero)=struct.unpack(FRAME_HEADER_FMT, data[0:8])
        assert zero == 0
        print "Got msg id=%d -- %d of %d" % (id, seq, total)
        print data[8:]

    def sendMsg(self, msg):
        id = self.__nextId()
        num=int(math.ceil(len(msg) / float(self.msgSize)))
        print "Going to send (%d) %d packet(s) for %d bytes" \
            % (id, num, len(msg))
        for p in range(num):
            start=p * self.msgSize
            end=min((p+1) * self.msgSize, len(msg))

            data=msg[start:end]
            assert len(data) <= self.msgSize

            hdr=struct.pack(FRAME_HEADER_FMT, id, p, num, 0)
            self.socket.sendto(hdr + data, self.remote)

if __name__ == '__main__':
    c=MemcachedHost(sys.argv[1])

    req="stats\r\n"
    if len(sys.argv) > 3:
        key, path=sys.argv[2:]
        f=open(path)
        stuff=f.read()
        f.close()
        req="set %s 0 30 %d\r\n%s\r\n" % (key, len(stuff), stuff)
    elif len(sys.argv) > 2:
        key=sys.argv[2]
        req="get %s\r\n" % (key,)
    else:
        print "I could use a few more args"
        sys.exit(1)

    c.sendMsg(req)
    c.getMsg()
