/*
 * Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
 *
 * $Id: PhotoSecurity.java,v 1.5 1999/10/20 02:14:57 dustin Exp $
 */

import java.security.*;
import java.util.*;
import java.sql.*;
import sun.misc.*;

public class PhotoSecurity extends PhotoHelper {
	// Secret string to verify authentication with
	protected String secret=null;
	protected Hashtable userdb=null;

	public PhotoSecurity(String key) throws Exception {
		super();
		secret = key;
	}

	public void setUserHash(Hashtable h) {
		userdb=h;
	}

	public String setAuthUser(String username) {
		String out = new String();
		try {
			out = "user." + username;
			String sig = sign(out);
			out += "^auth." + sig;
		} catch(Exception e) {
			// Nothin', just give no credentials.
		}
		return(out);
	}

	public String getAuthUser(String auth_info) {
		String username = "guest";
		try {
			Hashtable pieces = parseAuth(auth_info);
			String todigest=new String();
			todigest += "user." + (String)pieces.get("user");
			String fromdigest = sign(todigest);
			if(fromdigest.equals((String)pieces.get("auth"))) {
				username=(String)pieces.get("user");
			}
		} catch(Exception e) {
			// log("Got exception in PhotoSecurity:  " + e);
			// e.printStackTrace();
			// Nothing, just return guest.
		}

		return(username);
	}

	public static String generateSecret() {
		SecureRandom sr = new SecureRandom();
		byte data[] = new byte[32];
		sr.nextBytes(data);
		BASE64Encoder base64=new BASE64Encoder();
		String out = base64.encodeBuffer(data);
		out = out.replace('+', '/');
		return(out);
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

	// Generate a digest for a string.
	protected String sign(String input) throws Exception {
		byte dataB[]=input.getBytes();
		byte secretB[]=secret.getBytes();
		MessageDigest md = MessageDigest.getInstance(conf.get("cryptohash"));
		md.update(secretB);
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

	protected Hashtable parseAuth(String auth_info) throws Exception {
		Hashtable ht = new Hashtable();
		String pieces[] = PhotoUtil.split("^", auth_info);

		for(int i = 0; i<pieces.length; i++) {
			String pair[] = PhotoUtil.split(".", pieces[i]);
			ht.put(pair[0], pair[1]);
		}

		return(ht);
	}
}
