/*
 * Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
 *
 * $Id: PhotoSecurity.java,v 1.2 2000/03/08 07:18:12 dustin Exp $
 */

package net.spy.photo;

import java.security.*;
import java.util.*;
import java.sql.*;
import sun.misc.*;

import net.spy.*;

public class PhotoSecurity extends PhotoHelper {
	// Secret string to verify authentication with
	protected Hashtable userdb=null;

	public PhotoSecurity() throws Exception {
		super();
	}

	public void setUserHash(Hashtable h) {
		userdb=h;
	}

	// Verify a password against what is stored in the database.
	public boolean checkPW(String user, String pass) {
		boolean ret=false;
		Connection db=null;
		try {
			String tpw=getDigest(pass);
			String pw=null;

			// We can use the precached user database if it's there.
			if(userdb == null) {
				log("No userdb hash, getting auth info from database");
				String u=PhotoUtil.dbquote_str(user);
				db=getDBConn();
				Statement st=db.createStatement();
				String query = "select password from wwwusers where username ="
					+ " '" + u + "'";
				ResultSet rs = st.executeQuery(query);
				rs.next();
				pw = rs.getString("password");
			} else {
				log("Getting auth info from userdb hash");
				PhotoUser pu = (PhotoUser)userdb.get(user);
				if(pu != null) {
					pw = pu.password;
				}
			}
			log("Testing for " + tpw + " = " + pw);
			ret=tpw.equals(pw);
		} catch(Exception e) {
			// Nothing.
		} finally {
			try {
				freeDBConn(db);
			} catch(Exception e2) {
				// Nothing.
			}
		}
		return(ret);
	}

	// Get a digest for a string
	public String getDigest(String input) throws Exception {
		byte dataB[]=input.getBytes();
		MessageDigest md = MessageDigest.getInstance(conf.get("cryptohash"));
		md.update(dataB);
		BASE64Encoder base64=new BASE64Encoder();
		String out = base64.encodeBuffer(md.digest());
		out = out.replace('+', '/');
		out=out.trim();
		// Get rid of = signs.
		while(out.endsWith("=")) {
			out=out.substring(0,out.length()-1);
		}
		return(out.trim());
	}
}
