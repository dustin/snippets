/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoUtil.java,v 1.4 1999/10/05 04:12:00 dustin Exp $
 */

import java.lang.*;
import java.util.*;
import java.text.*;

// The class
public class PhotoUtil
{
	// Split that shit
	public static String[] split(String on, String input) {
		Vector v = new Vector();
		StringTokenizer st = new StringTokenizer(input, on);
		String ret[];
		int i;

		while( st.hasMoreTokens() ) {
			v.addElement(st.nextToken());
		}

		ret=new String[v.size()];

		for(i=0; i<v.size(); i++) {
			ret[i]=(String)v.elementAt(i);
		}

		return(ret);
	}

	// Make a strings safe for the database.
	public static String dbquote_str(String thing) {

		// Quick...handle null
		if(thing == null) {
			return(null);
		}

		String scopy = new String(thing);
		if(scopy.indexOf('\'') >= 0) {
			String sout = new String("");
			StringTokenizer st = new StringTokenizer(scopy, "\'");
			while(st.hasMoreTokens()) {
				String part = st.nextToken();

				if(st.hasMoreTokens()) {
					sout += part + "\'\'";
				} else {
					sout += part;
				}
			}
			scopy=sout;
		}
		return(scopy);
	}

	// Tokenize a template file and return the tokenized stuff.
	public static String tokenize(PhotoServlet p, String file, Hashtable vars) {
		Toker t=new Toker();
		String ret;

		vars.put("SELF_URI", p.self_uri);
		vars.put("HTML_URI", "/~dustin/jphoto/");
		vars.put("REMOTE_USER", p.remote_user);
		vars.put("REMOTE_UID", p.remote_uid.toString());
		vars.put("LAST_MODIFIED", "recently");
		vars.put("STYLESHEET", "<link rel=\"stylesheet\"href=\""
			+ "/servlet/root/PhotoServlet?func=getstylesheet\">");

		ret = t.tokenize("/home/dustin/public_html/jphoto/inc/" + file, vars);
		return(ret);
	}

	// Get today's date as a string
	public static String getToday() {
		Date ts=new Date();
		SimpleDateFormat f=new SimpleDateFormat("MM/dd/yyyy");
		return(f.format(ts));
	}
}
