#!/usr/bin/env python
"""
Trivial DHT.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 5FCA73BF-DD93-4E9E-BAF1-992F04CFEB4E

import sys
import xmlrpclib
import SimpleXMLRPCServer

class HashServices (object):
    "Basic hash services."

    def __init__(self, nodes):
        self.nodes=[]
        for n in nodes:
            if n == 'self':
                self.nodes.append(self)
            else:
                self.nodes.append(xmlrpclib.Server(n))
        self.storage={}

    def storeLocal(self, key, value):
        "Store the given value under the given key."
        self.storage[key]=value
        return True

    def getLocal(self, key):
        "Get the value for the given key."
        return self.storage[key]

    def keysLocal(self):
        "get the keys from this node"
        return self.storage.keys()

    def containsLocal(self, key):
        "Return true if this node contains the given key."
        return key in self.storage

    def deleteLocal(self, key):
        "Delete the item at the given key."
        del self.storage[key]
        return True

    def __getNode(self, h):
        return self.nodes[h % len(self.nodes)]

    def get(self, key):
        "Get value for the given key within the cluster."
        return self.__getNode(hash(key)).getLocal(key)

    def store(self, key, value):
        "Store the given value under the given key in the cluster."
        return self.__getNode(hash(key)).storeLocal(key, value)

    def keys(self):
        "Get all of the keys from all of the nodes."
        rv=[]
        for n in self.nodes:
            rv.extend(n.keysLocal())
        return rv

    def contains(self, key):
        "True if the cluster contains the given key."
        return self.__getNode(hash(key)).containsLocal(key)

    def delete(self, key):
        "Delete the given key from this cluster."
        return self.__getNode(hash(key)).deleteLocal(key)

class DictInterface (object):
    "A dictionary interface to a DHT cluster."

    def __init__(self, nodes):
        self.nodes = [xmlrpclib.Server(x) for x in nodes]

    def __getNode(self, k):
        return self.nodes[hash(k) % len(self.nodes)]

    def __contains__(self, k):
        return self.__getNode(k).contains(k)

    def __getitem__(self, k):
        return self.__getNode(k).get(k)

    def __setitem__(self, k, v):
        return self.__getNode(k).store(k, v)

    def __delitem__(self, k):
        return self.__getNode(k).delete(k)

    def keys(self):
        return self.nodes[0].keys()

if __name__ == '__main__':
    ip, port, urls=sys.argv[1], int(sys.argv[2]), sys.argv[3:]
    s=SimpleXMLRPCServer.SimpleXMLRPCServer((ip, port))
    s.register_introspection_functions()
    s.register_instance(HashServices(urls))
    s.serve_forever()
