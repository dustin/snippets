/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoLogView.java,v 1.1 1999/09/30 06:09:28 dustin Exp $
 */

import java.io.*;
import java.sql.*;
import java.util.*;
import sun.misc.*;

import com.javaexchange.dbConnectionBroker.*;

// The class
public class PhotoLogView extends PhotoHelper
{
	PhotoServlet photoservlet;

	public PhotoLogView(DbConnectionBroker db, PhotoServlet p) {
		super(db);
		photoservlet=p;
	}

	public String getViewersOf(Integer photo_id) throws Exception {
		Connection db;
		Statement st;
		String query, out="";

		try {
			db=dbs.getConnection();
		} catch(Exception e) {
			throw new Exception("Can't get database connection: "
				+ e.getMessage());
		}

		query = "select wwwusers.username, log.remote_addr, log.user_agent,\n"
			  + "    log.cached, log.ts\n"
			  + "  from wwwusers, log\n"
			  + "  where log.wwwuser_id = wwwusers.id and\n"
			  + "    log.photo_id = " + photo_id + "\n"
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
		}

		return(out);
	}
}
