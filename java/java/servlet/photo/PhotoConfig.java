/*
 * Copyright (C) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: PhotoConfig.java,v 1.9 1999/10/20 02:14:46 dustin Exp $
 */

import net.spy.*;

public class PhotoConfig extends SpyConfig {

	public PhotoConfig() {
		super("/tmp/config");
		orput("dbDriverName", "postgresql.Driver");
		// this.put("dbSource", "jdbc:postgresql://dhcp-104/photo");
		orput("dbSource", "jdbc:postgresql://localhost/photo");
		orput("dbUser", "nobody");
		orput("dbPass", "");
		// this.put("objectserver", "//localhost/RObjectServer");
		orput("objectserver", "//dhcp-104/RObjectServer");
		orput("includes", "/home/dustin/public_html/jphoto/inc/");
		orput("html_uriroot", "/~dustin/jphoto/");
		orput("timezone", "GMT");
		orput("cryptohash", "SHA");
	}
}
