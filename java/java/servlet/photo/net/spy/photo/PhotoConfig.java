/*
 * Copyright (C) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: PhotoConfig.java,v 1.4 1999/11/26 05:28:21 dustin Exp $
 */

package net.spy.photo;

import net.spy.*;

public class PhotoConfig extends SpyConfig {

	public PhotoConfig() {
		super("/usr/local/etc/photoconfig.xml");
		orput("dbDriverName", "postgresql.Driver");
		orput("dbSource", "jdbc:postgresql://localhost/photo");
		orput("dbUser", "nobody");
		orput("dbPass", "");
		orput("objectserver", "//localhost/RObjectServer");
		orput("imageserver", "//localhost/ImageServer");
		orput("includes", "/home/dustin/public_html/jphoto/inc/");
		orput("html_uriroot", "/~dustin/jphoto/");
		orput("timezone", "GMT");
		orput("cryptohash", "SHA");
		orput("storer_sleep", "10");
	}
}
