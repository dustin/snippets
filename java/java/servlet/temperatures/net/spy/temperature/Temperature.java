/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: Temperature.java,v 1.1 2000/01/17 00:44:25 dustin Exp $
 */

package net.spy.temperature;

import java.io.*;
import java.util.*;
import java.net.*;
import sun.misc.*;

import javax.servlet.*;
import javax.servlet.http.*;

import net.spy.*;
import net.spy.net.*;

// The class
public class Temperature extends HttpServlet
{
	Hashtable temps = null;

	// The once only init thingy.
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		temps=new Hashtable();

		temps.put("machineroom",
			"http://keyhole/~dustin/temp/current/1013A51E00000035");
		temps.put("bedroom",
			"http://dhcp-104/~dustin/temp/current/1081841E000000DF");
		temps.put("backyard",
			"http://dhcp-104/~dustin/temp/current/10E8C214000000E4");
		temps.put("livingroom",
			"http://butterfly/~dustin/temp/current/10C8892A00000096");
	}

	// Do a GET request
	public void doGet (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException {
		String which=request.getParameter("temp");
		String url;
		double t;
		if(which==null) {
			throw new ServletException("Must have temp parameter");
		}

		url=(String)temps.get(which);

		if(url==null) {
			throw new ServletException("Unknown location: " + which);
		}

		try {
			HTTPFetch f = new HTTPFetch(url);
			String s;
			int temptmp;
			s=f.getData();
			t=Double.valueOf(s).doubleValue();
			temptmp=(int)(t*100.0);
			t=(double)temptmp/100;
		} catch(Exception e) {
			throw new ServletException("Error getting temperature:  " + e);
		}

		try {
			response.setContentType("text/plain");
			PrintWriter out=response.getWriter();
			out.print(t);
			out.close();
		} catch(Exception e) {
			throw new ServletException("Error writing output:  " + e);
		}
	}
}
