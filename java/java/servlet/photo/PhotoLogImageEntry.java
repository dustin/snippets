/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoLogImageEntry.java,v 1.2 1999/09/30 17:37:14 dustin Exp $
 */


import java.util.*;
import java.text.*;
import java.io.*;

import javax.servlet.*;
import javax.servlet.http.*;

public class PhotoLogImageEntry extends PhotoLogEntry {

	private int photo_id;
	private int wwwuser_id;
	private Date timestamp;
	private String remote_addr;
	private String server_host;
	private String user_agent;
	private boolean cached;

	public PhotoLogImageEntry(int u, int p, boolean c,
		HttpServletRequest request) {
		super();
		photo_id=p;
		wwwuser_id=u;
		cached=c;
		remote_addr=request.getRemoteAddr();
		server_host=request.getServerName();
		user_agent=request.getHeader("User-Agent");
		timestamp = new Date();
	}

	public String toString() {
		String r;
		SimpleDateFormat f = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");

		r="insert into photo_log(photo_id, wwwuser_id, remote_addr, "
			+ "server_host, user_agent, cached, ts) values("
			+ photo_id + ", " + wwwuser_id + ", '" + remote_addr
			+ "', '" + server_host
			+ "', get_agent('" + PhotoUtil.dbquote_str(user_agent) + "'), '"
			+ cached + "', '" + f.format(timestamp) + " GMT')";

		return(r);
	}
}
