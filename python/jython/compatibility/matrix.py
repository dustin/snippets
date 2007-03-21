#!/usr/bin/env jython

import getdb

import java
import com

class UpdateableFile:

    def __init__(self):
        pass

    def getTypeId(self):
        raise "NotImplemented"

class AppsIni(UpdateableFile):

    def __init__(self):
        UpdateableFile.__init__(self)

    def getTypeId(self):
        return 1

class MatrixSingleton:

    def __init__(self):
        self.byGroup={}

    def getMatrix(self, fileType, group):
        rv = None
        if not self.byGroup.has_key((fileType,group)):
            self.__initGroup(fileType, group)
        rv=self.byGroup[(fileType, group)]

        return rv

    def __initGroup(self, fileType, group):
        conn=getdb.getDBConn()
        pst=conn.prepareStatement(
            """select * from tbl_FileVersion_map_ref 
                where FileType_num_fk = ? and Group_idnum_fk = ?""")
        pst.setInt(1, fileType.getTypeId())
        pst.setInt(2, group.getId())

        matrix=com.twowire.compatibility.VersionMatrix()

        rs=pst.executeQuery()
        while rs.next():
            low=rs.getInt("CompatibilityFrom_num_fk")
            if rs.wasNull():
                low=None
            else:
                low=java.lang.Integer(low)
            high=rs.getInt("CompatibilityTo_num_fk")
            if rs.wasNull():
                high=None
            else:
                high=java.lang.Integer(high)
            vr=com.twowire.compatibility.VersionRange(
                low, high, java.lang.Integer(rs.getInt("FileVersion_num")))
            matrix.add(vr)
        rs.close()
        pst.close()
        conn.close()

        self.byGroup[(fileType, group)]=matrix

if __name__ == '__main__':
    mf=MatrixSingleton()

    for groupSpec in ('global_mstr', 96):

        print "Working on", groupSpec

        g=com.twowire.group.Group.getGroup(groupSpec)
        matrix=mf.getMatrix(AppsIni(), g)

        print "Matrix:"
        print matrix

        for n in range(44,47):
            print "\tFetching", n
            o=matrix.find(java.lang.Integer(n))
            print "\t", o
            if o is not None:
                print "\t\t (",o.getCarryOn(), ")"

    print "All matrices:"
    print mf.byGroup
