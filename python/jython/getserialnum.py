#!/usr/bin/env jython

import getdb

query="""
	select
			owner.firstname, owner.lastname,
			mfg.serialnumber
		from
			tbl_owner owner, tbl_hpmanufacturingdata mfg,
			tbl_hpowner_map map
		where
			owner.organization_num_fk = 74
			and owner.owner_idnum_pk = map.owner_idnum_fk
			and mfg.hpbox_num_pk = map.hpbox_num_fk
"""

conn=getdb.getDBConn()
st=conn.createStatement()
rs=st.executeQuery(query)
while rs.next():
	print rs.getString(1)
	print rs.getString(2)
	print rs.getString(3)
	print "--------"
