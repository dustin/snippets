/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: TestTaker.java,v 1.1 2001/01/27 09:14:10 dustin Exp $
 */

package net.spy.test;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.net.*;
import sun.misc.*;

import javax.servlet.*;
import javax.servlet.http.*;

import net.spy.*;

// The class
public class TestTaker extends HttpServlet
{ 
	// The once only init thingy.
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		SpyConfig c = new TestConfig();
		log("Config:  " + c);
	}

	// Do a GET request
	public void doGet (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {

		TestSession ts = new TestSession(this, request, response);
		ts.process();
	}

	// Do a POST request
	public void doPost (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {

		TestSession ts = new TestSession(this, request, response);
		ts.process();
	}
}
