/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: Init.java,v 1.1 2002/02/20 09:17:34 dustin Exp $
 */

package net.spy.initservlet;

import java.io.*;
import java.net.*;
import java.util.*;

import javax.servlet.*;
import javax.servlet.http.*;

/**
 * Perform global servlet engine initialization.
 */
public class Init extends HttpServlet
{

	/**
	 * Just perform basic initialization.
	 */
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		Locale.setDefault(Locale.US);
		Locale l=Locale.getDefault();

		log("Default locale is now " + l);
	}
}
