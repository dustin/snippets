/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoLogView.java,v 1.4 1999/10/12 22:54:09 dustin Exp $
 */

import java.io.*;
import java.sql.*;
import java.util.*;
import sun.misc.*;

// The class
public class PhotoLogView extends PhotoHelper
{
	PhotoServlet photoservlet;

	public PhotoLogView(PhotoServlet p) throws Exception {
		super();
		photoservlet=p;
	}

	public String getViewersOf(Integer photo_id) throws Exception {
		Connection db;
		Statement st;
		String query, out="";

		try {
			db=getDBConn();
		} catch(Exception e) {
			throw new Exception("Can't get database connection: "
				+ e.getMessage());
		}

		query = "select wwwusers.username, log.remote_addr,\n"
			  + "   user_agent.user_agent, log.cached, log.ts\n"
			  + "  from wwwusers, photo_log log, user_agent\n"
			  + "  where log.wwwuser_id = wwwusers.id and\n"
			  + "    log.photo_id = " + photo_id + " and\n"
			  + "    user_agent.user_agent_id = log.user_agent\n"
			  + "  order by log.ts\n";
		try {
			st = db.createStatement();
			ResultSet rs = st.executeQuery(query);

			Hashtable htmp = new Hashtable();
			htmp.put("PHOTO_ID", photo_id.toString());
			out=PhotoUtil.tokenize(photoservlet, "log/viewers_top.inc", htmp);

			while(rs.next()) {
				Hashtable h = new Hashtable();
				h.put("USERNAME", rs.getString(1));
				h.put("REMOTE_ADDR", rs.getString(2));
				h.put("USER_AGENT", rs.getString(3));
				h.put("CACHED", rs.getString(4));
				h.put("TS", rs.getString(5));
				out+=PhotoUtil.tokenize(photoservlet, "log/viewers_match.inc",
					h);
			}

			out+=PhotoUtil.tokenize(photoservlet, "log/viewers_bottom.inc",
				new Hashtable());

		} catch(Exception e) {
			throw new Exception(e.getMessage());
		} finally {
			freeDBConn(db);
		}

		return(out);
	}
}
