/*
 * Copyright (C) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: PhotoConfig.java,v 1.1 1999/10/20 03:42:57 dustin Exp $
 */

package net.spy.photo;

import net.spy.*;

public class PhotoConfig extends SpyConfig {

	public PhotoConfig() {
		super("/usr/local/etc/photo.conf");
		orput("dbDriverName", "postgresql.Driver");
		orput("dbSource", "jdbc:postgresql://dhcp-104/photo");
		// orput("dbSource", "jdbc:postgresql://localhost/photo");
		orput("dbUser", "nobody");
		orput("dbPass", "");
		// orput("objectserver", "//localhost/RObjectServer");
		orput("objectserver", "//dhcp-104/RObjectServer");
		orput("includes", "/home/dustin/public_html/jphoto/inc/");
		orput("html_uriroot", "/~dustin/jphoto/");
		orput("timezone", "GMT");
		orput("cryptohash", "SHA");
	}
}
