/*
 * Copyright (C) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: PhotoConfig.java,v 1.5 1999/10/10 20:47:29 dustin Exp $
 */

public class PhotoConfig extends Object {
	public String dbDriverName="postgresql.Driver";
	public String dbSource="jdbc:postgresql://dhcp-104/photo";
	// public String dbSource="jdbc:postgresql://localhost/photo";
	public String dbUser="nobody";
	public String dbPass="";

	public String objectserver="//dhcp-104/RObjectServer";

	public String cryptohash="SHA";
}
