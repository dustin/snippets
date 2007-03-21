/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoUtil.java,v 1.5 2000/05/01 04:32:37 dustin Exp $
 */

package net.spy.photo;

import java.lang.*;
import java.util.*;
import java.text.*;

import net.spy.*;

// The class
public class PhotoUtil
{ 
	// Split that shit
	public static String[] split(String on, String input) {
		return(SpyUtil.split(on, input));
	}

	// Make a strings safe for the database.
	public static String dbquote_str(String thing) {
		return(SpyDB.dbquote_str(thing));
	}

	// Tokenize a template file and return the tokenized stuff.
	public static String tokenize(PhotoSession p, String file, Hashtable vars) {
		SpyToker t=new SpyToker();
		String ret;

		PhotoConfig conf = new PhotoConfig();

		vars.put("SELF_URI", p.self_uri);
		vars.put("HTML_URI", conf.get("html_uriroot"));
		vars.put("REMOTE_USER", p.remote_user);
		vars.put("REMOTE_UID", p.remote_uid.toString());
		vars.put("LAST_MODIFIED", "recently");
		vars.put("STYLESHEET", "<link rel=\"stylesheet\"href=\""
			+ p.self_uri + "?func=getstylesheet\">");
		
		ret = t.tokenize(conf.get("includes") + file, vars);
		return(ret);
	}

	// Get today's date as a string
	public static String getToday() {
		Date ts=new Date();
		SimpleDateFormat f=new SimpleDateFormat("MM/dd/yyyy");
		return(f.format(ts));
	}
}
