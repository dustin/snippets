/*
 * Copyright (C) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: PhotoConfig.java,v 1.8 1999/10/19 06:24:47 dustin Exp $
 */

public class PhotoConfig extends Object {

	// Database Stuff
	public String dbDriverName="postgresql.Driver";
	public String dbSource="jdbc:postgresql://dhcp-104/photo";
	// public String dbSource="jdbc:postgresql://localhost/photo";
	public String dbUser="nobody";
	public String dbPass="";

	// Objectserver
	public String objectserver="//dhcp-104/RObjectServer";
	// public String objectserver="//localhost/RObjectServer";

	// Includes
	public String includes="/home/dustin/public_html/jphoto/inc/";

	// References
	public String html_uriroot="/~dustin/jphoto/";

	// Timezone that the servlet engine thinks we're in.
	public String timezone="GMT";

	public String cryptohash="SHA";
}
