#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: skeleton.py,v 1.2 2003/01/03 08:43:26 dustin Exp $
"""

import sys
import string
import com

import getdb

v5base='5225-2374-WHE2-22AZ-B2LW'

if __name__ == '__main__':
	baseKey=com.twowire.activation.Keycode.parseKeycode(v5base)

	conn=getdb.getDBConn()
	pst=conn.prepareStatement("""select V4Master_num_fk, V4ISP_num_fk
		from tbl_v4keycode where KeycodeString = ?""")

	f=open(sys.argv[1])
	for l in f.readlines():
		# Print comments to stderr
		if l[0] == '#':
			sys.stderr.write(l)
			continue
		l=l.rstrip()

		# Set the keycode string
		pst.setString(1, l);

		mo=None
		isp=None

		rs=pst.executeQuery()
		while rs.next():
			mo = rs.getInt("V4Master_num_fk")
			if rs.wasNull():
				mo = None
			else:
				try:
					# Look up the organization
					o=com.twowire.org.Organization.getOrganization(mo)
				except com.twowire.org.NoSuchOrganizationException:
					print "# No such org:  " + `mo` + " (looking at " + l + ")"
					mo = None

			isp = rs.getInt("V4ISP_num_fk")
			if rs.wasNull():
				isp = None

		rs.close()

		if mo is None or isp is None:
			print "# Nothing found for " + l + " in the DB"
			print l + "\t" + v5base
			continue

		print "# " + o.getName() + " (mo=" + str(mo) \
			+ ", isp=" + (str(isp)) + ")"
		baseKey.setOrgId(mo)
		baseKey.setIspId(isp)
		print l + "\t" + baseKey.toKeycodeString()
