#!/usr/bin/env jython

import getdb

query1="select sourceipaddress, hpipaddress from tbl_hpidentification " \
	+ " where sourceipaddress is not null or hpipaddress is not null"

conn=getdb.getDBConn()
st=conn.createStatement()
rs=st.executeQuery(query1)

count=0
diff=0
empty=0
while rs.next():
	s=rs.getString("sourceipaddress").rstrip()
	h=rs.getString("hpipaddress").rstrip()

	if s == '' and h == '':
		empty+=1
	else:
		if s!= h:
			diff+=1

		count+=1

		print s + "\t" + h

print "# Total IP addresses: ", count
print "# Different IP addresses: ", diff
print "# Empty IP addresses: ", empty
