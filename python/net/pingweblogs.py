#!/usr/bin/env python
"""

Copyright (c) 2006  Dustin Sallings <dustin@spy.net>
"""

import traceback
import xmlrpclib

sites=[
    ('Rockstar Programmer', 'http://www.rockstarprogrammer.org/rss/full/'),
    ('My Twisted Life', 'http://blog.west.spy.net/feed/full/noelani/'),
    ('Too Much Information', 'http://blog.west.spy.net/feed/full/dustin/'),
]

locations=(
    ( 'Weblogs.com', 'http://rpc.weblogs.com/RPC2' ),
    ( 'Feed Burner', 'http://ping.feedburner.com/' ),
    ( 'Feedster', 'http://api.feedster.com/ping' ),
    ( 'Blo.gs', 'http://ping.blo.gs/' ),
    ( 'Syndic8', 'http://ping.syndic8.com/xmlrpc.php' ),
    ( 'BlogRolling', 'http://rpc.blogrolling.com/pinger/' ),
    ( 'Technorati', 'http://rpc.technorati.com/rpc/ping' ),
    ( 'PubSub.com', 'http://xping.pubsub.com/ping/' )
)

for s in sites:
    for l in locations:
        w=xmlrpclib.Server(l[1])
        try:
            print "pinging %s against %s" % (s[0], l[0])
            print w.weblogUpdates.ping(s[0], s[1])
        except:
            traceback.print_exc()
