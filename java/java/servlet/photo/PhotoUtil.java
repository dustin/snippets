/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoUtil.java,v 1.1 1999/09/28 06:37:21 dustin Exp $
 */

import java.io.*;
import java.sql.*;
import java.text.*;
import java.util.*;
import java.net.*;
import sun.misc.*;

import javax.servlet.*;
import javax.servlet.http.*;

import com.oreilly.servlet.*;
import com.javaexchange.dbConnectionBroker.*;


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
}
