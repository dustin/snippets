/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: TestTaker.java,v 1.1 1999/11/15 08:39:26 dustin Exp $
 */

import java.io.*;
import java.sql.*;
import java.util.*;
import java.net.*;
import sun.misc.*;

import javax.servlet.*;
import javax.servlet.http.*;

import net.spy.*;
import net.spy.test.*;

// The class
public class TestTaker extends HttpServlet
{
	// The once only init thingy.
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
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
