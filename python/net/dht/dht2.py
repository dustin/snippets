#!/usr/bin/env python
"""
A slightly less trivial DHT.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import sys
import xmlrpclib
import SimpleXMLRPCServer

import node

class NodeService(node.LocalNode):
    """Services for communication across nodes."""

    def __init__(self, node_id):
        super(NodeService, self).__init__(node_id)

    def contains(self, key):
        rv=super(NodeService, self).contains(key)
        return xmlrpclib.boolean(rv)

    def set(self, key, value):
        super(NodeService, self).set(key, value)
        return xmlrpclib.boolean(True)

    def delete(self, key):
        rv=super(NodeService, self).delete(key)
        return xmlrpclib.boolean(rv)

if __name__ == '__main__':
    # ip, port, urls=sys.argv[1], int(sys.argv[2]), sys.argv[3:]
    ip, port, node_id=sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
    s=SimpleXMLRPCServer.SimpleXMLRPCServer((ip, port))
    s.register_introspection_functions()
    s.register_instance(NodeService(node_id))
    s.serve_forever()
