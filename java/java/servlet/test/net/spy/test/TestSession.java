/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: TestSession.java,v 1.1 1999/11/15 08:39:28 dustin Exp $
 */

package net.spy.test;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.net.*;
import sun.misc.*;

import javax.servlet.*;
import javax.servlet.http.*;

import com.oreilly.servlet.*;

import net.spy.*;

// The class
public class TestSession extends Object
{
	// This kinda stuff is only persistent for a single connection.
	HttpServletRequest request=null;
	HttpServletResponse response=null;
	HttpSession session=null;
	HttpServlet servlet=null;
	SpyConfig test_config=null;

	// What test someone is taking
	Integer test_no;

	// have we done this before?
	static boolean initialized=false;

	protected void log(String s) {
		servlet.log(s);
	}

	// The only way to get the session context is to have a session, so we
	// have to do it here.  That sucks, but it's what we got.
	protected synchronized void initialize() {
		// Yes, this test was already done, but this time, it's
		// synchronized.
		if(initialized==false) {
			initialized=true;
			TestSessionSweeper t =
				new TestSessionSweeper(session.getSessionContext());
			t.start();
		}
	}

	// Constructor.
	public TestSession(HttpServlet s,
		HttpServletRequest request,
		HttpServletResponse response) {

		this.request=request;
		this.response=response;
		servlet=s;
		session=request.getSession(true);
		test_config=new TestConfig();
		if(initialized==false) {
			initialize();
		}
	}

	// process a request
	public void process() throws ServletException {

		String func;
		if(session.isNew()) {
			log(session.getId() + " is a new session");
		} else {
			log(session.getId() + " is an old session");
		}

		// Get the test number we're operating on.
		test_no = (Integer)session.getValue("test_no");

		func=request.getParameter("func");
		if(func==null) {
			getTest();
		} else if(func.equalsIgnoreCase("settest")) {
			// setTest();
		} else {
			throw new ServletException("No known function.");
		}
	}


	protected void getTest() throws ServletException {
		if(test_no == null) {
			send_response(listTests());
		}
	}

	protected String listTests() throws ServletException {
		SpyDB db = new SpyDB(test_config, true);
		String ret="";

		ret+="<form method=\"POST\" action=\""
			+ request.getRequestURI() + "\">\n";
		ret+="<input type=\"hidden\" name=\"func\" value=\"settest\">\n";
		ret+="<select name=\"test_id\">\n";

		try {
			Connection tdb=db.getConn();
			Statement st = tdb.createStatement();
			ResultSet rs=st.executeQuery("select * from test");

			while(rs.next()) {
				String id = rs.getString(1);
				String test = rs.getString(2);
				ret+="<option value=\"" + id + "\">" + test + "\n";
			}
		} catch(Exception e) {
			// A valid, but empty string will be returned upon error.
		}
		ret+="</select>\n";
		ret+="<input type=submit value=Select>\n";
		ret+="</form>\n";
		return(ret);
	}

	protected void send_response(String text) {
		response.setContentType("text/html");
		try {
			PrintWriter out=response.getWriter();
			out.print(text);
			out.close();
		} catch(Exception e) {
			// Yeah, yeah
		}
	}
}
