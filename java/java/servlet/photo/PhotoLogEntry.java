// Object to store the shite properly.

import java.util.*;
import java.text.*;
import java.io.*;

import javax.servlet.*;
import javax.servlet.http.*;

public class PhotoLogEntry extends Object {

	private int photo_id;
	private int wwwuser_id;
	private Date timestamp;
	private String remote_addr;
	private String server_host;
	private String user_agent;

	public PhotoLogEntry(int p, int u, HttpServletRequest request) {
		super();
		photo_id=p;
		wwwuser_id=u;
		remote_addr=request.getRemoteAddr();
		server_host=request.getServerName();
		// user_agent=getAttribute("org.apache.jserv.USER_AGENT");
		user_agent=request.getHeader("User-Agent");
		timestamp = new Date();
	}

	public String toString() {
		String r;
		SimpleDateFormat f = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");

		r="insert into log(photo_id, wwwuser_id, remote_addr, "
			+ "server_host, user_agent, ts) values("
			+ photo_id + ", " + wwwuser_id + ", '" + remote_addr
			+ "', '" + server_host + "', '" + PhotoUtil.dbquote_str(user_agent)
			+ "', '" + f.format(timestamp) + "')";

		return(r);
	}
}
