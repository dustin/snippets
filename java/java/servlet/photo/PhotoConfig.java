/*
 * Copyright (C) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: PhotoConfig.java,v 1.6 1999/10/10 20:58:43 dustin Exp $
 */

public class PhotoConfig extends Object {

	// Database Stuff
	public String dbDriverName="postgresql.Driver";
	// public String dbSource="jdbc:postgresql://dhcp-104/photo";
	public String dbSource="jdbc:postgresql://localhost/photo";
	public String dbUser="nobody";
	public String dbPass="";

	// Objectserver
	// public String objectserver="//dhcp-104/RObjectServer";
	public String objectserver="//localhost/RObjectServer";

	// Includes
	public String includes="/home/dustin/public_html/jphoto/inc/";

	// References
	public String html_uriroot="/~dustin/jphoto/";


	public String cryptohash="SHA";
}
