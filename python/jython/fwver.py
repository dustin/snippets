#!/usr/bin/env jython

import com
import java

import time

import getdb
import lookupSerial

myserialnumber='400000000334'

class FWStuff:
    def __init__(self):
        pass

    def go(self):
        gws=lookupSerial.GatewaySerial(myserialnumber)
        gwb=gws.getGatewayBean()

        db=getdb.getDBConn(db='Aztec6')

        query="select * from tbl_HP where HPBox_num_fk_pk=" \
            + `gwb.getHPBoxNum()`
        st=db.createStatement()
        rs=st.executeQuery(query)

        getdb.dumpResults(rs)

if __name__ == '__main__':
    x=FWStuff()
    x.go()
