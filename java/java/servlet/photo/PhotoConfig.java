/*
 * Copyright (C) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: PhotoConfig.java,v 1.4 1999/10/10 19:44:18 dustin Exp $
 */

public class PhotoConfig extends Object {
	public String dbDriverName="postgresql.Driver";
	public String dbSource="jdbc:postgresql://dhcp-104/photo";
	// public String dbSource="jdbc:postgresql://localhost/photo";
	public String dbUser="nobody";
	public String dbPass="";

	public String cryptohash="SHA";
}
