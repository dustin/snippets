#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: skeleton.py,v 1.2 2003/01/03 08:43:26 dustin Exp $
"""

import java
import com
import net

# Aliases
b64=net.spy.util.Base64()
cbais=java.io.ByteArrayInputStream
cois=java.io.ObjectInputStream

md=java.security.MessageDigest.getInstance("MD5")
cdbpath="/tw/var/dcache/manufacturing.cdb"

enumerator=com.strangegizmo.cdb.Cdb.elements(cdbpath)
while enumerator.hasMoreElements():
	e=enumerator.nextElement()

	ois=None
	try:
		ois=cois(cbais(e.getData()))
		mfg=ois.readObject()
	finally:
		if ois is not None:
			ois.close()

	sn = mfg.getSerialNumber()
	idString = mfg.getIdString()

	md.reset()
	md.update(java.lang.String(idString + sn).getBytes())
	hashedIdString=b64.encode(md.digest())

	print sn + "\t" + idString + "\t" + hashedIdString
