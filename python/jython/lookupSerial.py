#!/usr/bin/env jython

import getdb
import sys

import com
import java

class GatewaySerial:

	def __init__(self, serialnumber):
		self.sn=serialnumber
		query="select * from tbl_hpidentification " \
			+ " where serialnumber_pk = '" + serialnumber + "'"

		conn=getdb.getDBConn()
		st=conn.createStatement()
		rs=st.executeQuery(query)
		rsmd=rs.getMetaData()

		if not rs.next():
			raise "No results"

		self.__fields={}
		self.__columns=[]
		for i in range(rsmd.getColumnCount()):
			columnname=str(rsmd.getColumnName(i+1))
			self.__columns.append(columnname)
			self.__fields[columnname]=str(rs.getString(i+1))

		if rs.next():
			raise "Too many results"

		rs.close()
		st.close()
		conn.close()

	def keys(self):
		return self.__columns

	def __getitem__(self, which):
		return self.__fields[which]

	def __repr__(self):
		return "<GatewaySerial " + repr(self.__fields) + ">"

	def getHeartbeatStruct(self, event, command):
		hbs=com.twowire.rpc.struct.HeartbeatStruct()

		hbs.setEvent(event)
		hbs.setCommandKey(command)
		hbs.setIPAddress(self['HPIPAddress'])
		hbs.setPcAssemblyNumber(self['PcAssemblyNumber'])
		hbs.setSoftwareDottedString(self['VersionDottedString'])
		hbs.setCurrentTime(java.util.Date())
		hbs.setKeycode(self['KeycodeString'])

		return(hbs)

	def getGatewayBean(self):
		return(com.twowire.gateway.GatewayBean.createGatewayFromSerial(self.sn))

if __name__ == '__main__':
	gw=GatewaySerial(sys.argv[1])

	for k in gw.keys():
		print k + ":  " + gw[k]
