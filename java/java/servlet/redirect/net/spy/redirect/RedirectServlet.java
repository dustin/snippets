// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: RedirectServlet.java,v 1.1 2002/05/05 07:47:03 dustin Exp $

package net.spy.redirect;

import java.io.*;
import java.net.*;

import javax.servlet.*;
import javax.servlet.http.*;

/**
 * Generic redirect servlet.
 */
public class RedirectServlet extends HttpServlet {

	private String target=null;

	/**
	 * Initialize the servlet.
	 */
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		target=config.getInitParameter("target");
	}

	/**
	 * Issue a redirect to the target.
	 */
	protected void doGet(
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException, IOException {

		response.sendRedirect(target);
	}

}
