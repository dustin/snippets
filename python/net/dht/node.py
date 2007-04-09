#!/usr/bin/env python
"""
A node in my DHT.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import xmlrpclib
import exceptions

class Node(object):
    """A node within a dht."""
    node_id=None
    storage=None

    def __init__(self, node_id):
        """Construct a node with the given node id and dict-like storage."""
        self.node_id=node_id

    def __repr__(self):
        return "<Node id=%d>" % (self.node_id)

    def get(self, key):
        """Retrieve the value for a key."""
        raise exceptions.NotImplementedError

    def set(self, key, value):
        """Store a value for this key."""
        raise exceptions.NotImplementedError

    def delete(self, key):
        """Delete the item with the given key."""
        raise exceptions.NotImplementedError

    def contains(self, key):
        """True if this node contains the given key."""
        raise exceptions.NotImplementedError

    def getId(self):
        """Get the ID of this node."""
        return self.node_id

class LocalNode(Node):
    """A local (in-process) node."""

    storage=None

    def __init__(self, node_id, storage={}):
        super(LocalNode, self).__init__(node_id)
        self.storage=storage

    def get(self, key):
        """Retrieve the value for a key."""
        return self.storage[key]

    def set(self, key, value):
        """Store a value for this key."""
        self.storage[key]=value

    def delete(self, key):
        """Delete the item with the given key."""
        del self.storage[key]

    def contains(self, key):
        """True if this node contains the given key."""
        return key in self.storage

class RemoteNode(Node):
    """A remote node within a DHT."""

    rpcproxy=None

    def __init__(self, node_id, url):
        """Construct a remote node located at the given URL."""
        super(RemoteNode, self).__init__(node_id)
        self.rpcproxy=xmlrpclib.Server(url)
        assert self.rpcproxy.getId() == node_id

    def get(self, key):
        """Retrieve the value for a key."""
        return self.rpcproxy.get(key)

    def set(self, key, value):
        """Store a value for this key."""
        self.rpcproxy.set(key, value)

    def delete(self, key):
        """Delete the item with the given key."""
        return self.rpcproxy.delete(key)

    def contains(self, key):
        """True if this node contains the given key."""
        return self.rpcproxy.contains(key)
