/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: Temperature.java,v 1.3 2000/03/16 06:20:10 dustin Exp $
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
	Properties temps = null;

	// The once only init thingy.
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		temps=new Properties();
		try {
			temps.load(new
				FileInputStream("/usr/local/etc/temperature.properties"));
		} catch(Exception e) {
			throw new ServletException("Error loading properties:  " + e);
		}

	}

	// Do a GET request
	public void doGet (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException {
		String which=request.getParameter("temp");
		String out="";
		if(which==null) {
			out=listTemps();
		} else {
			out=getTemp(which);
		}

		send_response(response, out);
	}

	protected String listTemps() {
		String ret="";

		for(Enumeration e = temps.keys(); e.hasMoreElements();) {
			ret+=e.nextElement() + "\n";
		}

		return(ret);
	}

	protected String getTemp(String which) throws ServletException {
		String url=(String)temps.get(which);
		double t;
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
		return("" + t);
	}

	protected void send_response(HttpServletResponse response, String o) {
		try {
			response.setContentType("text/plain");
			PrintWriter out=response.getWriter();
			out.print(o);
			out.close();
		} catch(Exception e) {
			// Ignore.
		}
	}
}
