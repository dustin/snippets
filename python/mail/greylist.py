#!/usr/bin/env python
"""
Greylist filter plugin for postfix

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: F639689B-3486-11D9-99DF-000A957659CC

import logging
import logging.handlers
import postfix
import lockeddbm

DBPATH="/var/tmp/greylist"

if __name__ == '__main__':
    # Configure up logging
    handler=logging.handlers.SysLogHandler(
        facility=logging.handlers.SysLogHandler.LOG_MAIL)
    handler.setFormatter(logging.Formatter(logging.BASIC_FORMAT))
    logging.root.addHandler(handler)
    logging.root.setLevel(logging.INFO)

    # Set up the storage
    db=lockeddbm.LockedDBM(DBPATH, "c")

    # Set up the engine
    engine=postfix.GreylistPolicyEngine(db)

    # Run it forever
    engine.run()
