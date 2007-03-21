#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: dbrecorder.py,v 1.1 2003/09/26 01:13:21 dustin Exp $
"""

import sys

import com
import net
import java

class Transaction(com.twowire.database.SimpleSavableImpl):

    def __init__(self, lookups, tid, processor):
        self.lookups=lookups
        self.tid=tid
        self.processor=processor
        self.now=java.lang.System.currentTimeMillis()

    def save(self, conn, context):
        sn=self.processor.getSerialNumber()
        m=self.processor.getResults()

        itxl=com.twowire.database.sp.app.mdcscrape.InsertTxnList(conn)
        itxl.setSerialNumber(sn)
        itxl.setTxnKey(self.tid.intValue())
        ts=m['endDate']
        if ts is None:
            ts = java.util.Date()
        itxl.setCreatedDate(java.sql.Timestamp(ts.getTime()))
        itxl.executeUpdate()
        itxl.close()

        idv=com.twowire.database.sp.app.mdcscrape.InsertDataValue(conn)
        idv.setTxnKey(self.tid.intValue())
        for v in self.processor.getResults().values():
            if isinstance(v, java.util.Map):
                for e in v.entrySet():
                    lk=e.getKey()
                    lv=e.getValue()

                    if self.lookups.containsKey(lk):
                        dk=self.lookups[lk]
                        idv.setDataKey(dk)
                        idv.setValue(lv)
                        # print "Storing " + `dk` + " as " + lv
                        idv.executeUpdate()
                    else:
                        if not lk == 'components':
                            print "NO KEY FOR " + `lk` + " would have stored " \
                                + `lv`

        idv.close()

        self.setSaved()

class DBRecorder(com.twowire.app.mdcscrape.AbstractMDCRecorder):

    def __init__(self, conf):
        self.conf=conf
        gl=com.twowire.app.mdcscrape.MDCLookup.getInstance()
        self.le=gl.getLookupEntries(conf, "data_list", "data_key") 
        self.gpk=net.spy.db.GetPK.getInstance()
        self.tp=com.twowire.database.TransactionPipeline.getInstance()
        self.tp.setBlockSize(200)

    def __getTransId(self):
        return self.gpk.getPrimaryKey(self.conf, "txn_list")

    def reportSuccessfulJob(self, p):
        id=self.__getTransId()
        # print "DBRecorder sees " + `p` + " which will be txn_id: " + `id`
        self.tp.addTransaction(Transaction(self.le, id, p), self.conf)

    def finished(self):
        print "Finished with the DB recorder"
        com.twowire.database.TransactionPipeline.shutdown()

if __name__ == '__main__':
    conf=net.spy.SpyConfig(sys.argv[1])
    print conf
    dbconf=net.spy.SpyConfig(sys.argv[2])
    print dbconf
    howMany=int(sys.argv[3])

    recorder=DBRecorder(dbconf)

    main=com.twowire.app.mdcscrape.MDCMain(conf)
    main.addRecorder(recorder)
    main.go(howMany, sys.stdin)

# for i in range(100):
