#!/usr/bin/env jython

import java

def logTest(logthing):
    logger=java.util.logging.Logger.getLogger(logthing)

    logger.finest(logthing + " This log entry is *fine*!")
    logger.finer(logthing + " This log entry is kinda fine.")
    logger.fine(logthing + " This log entry is alright.")
    logger.config(logthing + " Not sure what this config shite is all about.")
    logger.info(logthing + " FYI")
    logger.warning(logthing + " I'm warning you!")
    logger.severe(logthing + " SHIT!")
    logger.log(java.util.logging.Level.SEVERE,
        logthing + " Shit stack", java.lang.Exception("help!"))

    logger.severe("----------------------------------")

logTest("py.py1")
logTest("py.py2")
