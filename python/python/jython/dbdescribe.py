#!/usr/bin/env jython

import sys

import getdb

import java
import net

class Table:

    def __init__(self, conn, table):
        self.conn=conn
        self.table=table

        pkg="com.twowire.database.sp.generated"
        if self.table.find("tbl_") == 0:
            pkg+=".old"
        self.pkg=pkg

        # Custom column type mapping
        self.colTypes={}
        self.colTypes[-9]='VARCHAR'
        self.colTypes[-8]='VARCHAR'
        self.colTypes[-10]='LONGVARCHAR'

    def __repr__(self):
        return "<Table: " + self.table + ">"

    def getTableName(self):
        return self.table

    def getPackageName(self):
        return self.pkg

    def columnType(self, t):
        rv=None
        if self.colTypes.has_key(t):
            rv=self.colTypes[t]
        else:
            rv=net.spy.db.TypeNames.getTypeName(t)
        return rv

    def toDBSP(self):
        rv=""

        st=conn.createStatement()
        query="select * from " + self.table + " where 1=0"
        rs=st.executeQuery(query)
        rs.next()
        rsmd=rs.getMetaData()

        rv+="@package\n" + self.pkg + "\n\n"
        rv+="@description\nSelect all rows from " + self.table + ".\n\n"

        rv+="@sql\n"
        rv+="select\n"
        for i in range(1, rsmd.getColumnCount()+1):
            rv+="\t\t" + rsmd.getColumnName(i)
            if i < rsmd.getColumnCount():
                rv+=","
            rv+="\n"
        rv+="\tfrom\n\t\t" + self.table + "\n\n"

        rv+="@results\n"

        for i in range(1, rsmd.getColumnCount()+1):
            nullable="(nullable)"
            if rsmd.isNullable(i) == rsmd.columnNullable:
                nullable="(not nullable)"
            rv+=rsmd.getColumnName(i) + " " \
                + self.columnType(rsmd.getColumnType(i)).upper() \
                + " " + rsmd.getColumnName(i) + " " + nullable + "\n"

        return rv

class DescribeDB:

    def __init__(self, conn):
        self.conn=conn

    def describeIt(self):
        rs=self.conn.getMetaData().getTables(None, None, None, None)
        tables=[]
        while rs.next():
            if rs.getString("TABLE_TYPE") == "TABLE":
                tables.append(Table(conn, rs.getString("TABLE_NAME")))
        rs.close()

        return tables

def titleize(s):
    rv=s[0].upper() + s[1:]
    return rv

if __name__ == '__main__':
    conn=getdb.getDBConn()
    d=DescribeDB(conn)
    tables=d.describeIt()

    basedir=sys.argv[1]

    print "Basedir:  " + basedir

    for i in tables:
        tn=i.getTableName()
        tnp=tn.split('_')
        tn=''.join(map(titleize, tnp))
        fqfn=basedir + "/" + i.getPackageName().replace(".", "/") \
            + "/SelectAll" + tn + ".spt"

        print "Writing " + fqfn
        f=open(fqfn, "w")
        f.write(i.toDBSP())
        f.close()
