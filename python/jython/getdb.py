#!/usr/bin/env jython

import sys

import java.sql.DriverManager


def getDBConn(host='engsql.eng.2wire.com', db='Aztec13',
        username='cms2wire', password='2wire'):
    driver="com.newatlanta.jturbo.driver.Driver"
    url="jdbc:JTurbo://" + host + ":1433/" \
        + db + "/sql70=true:UID=" + username + ":" + "PWD=" + password

    tmp=java.lang.Class.forName(driver)

    conn=java.sql.DriverManager.getConnection(url, 'cms2wire', '2wire')
    return conn

def dumpResults(rs):
    rsmd=rs.getMetaData()

    rn=0
    while rs.next():
        print " *** ROW " + str(rn) + " ***"
        for i in range(1, rsmd.getColumnCount()+1):
            print str(rsmd.getColumnName(i)) + " = " + str(rs.getString(i))
        print ""
        rn+=1

    if rn == 0:
        print " *** NO RESULTS, BUT THERE WERE COLUMNS ***"
        for i in range(1, rsmd.getColumnCount()+1):
            print rsmd.getColumnName(i)

if __name__ == '__main__':
    conn=getDBConn(host='demosql.eng.2wire.com', db='Aztec13')
    st=conn.createStatement()
    query="""select VersionCompatibility_num_fk, VersionDottedString
        from tbl_version
        order by VersionCompatibility_num_fk, VersionDottedString"""
    rs=st.executeQuery(query)

    dumpResults(rs)
