// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: SafetyServlet.java,v 1.1 2001/01/29 06:45:41 dustin Exp $

package net.spy.safety;

import javax.servlet.*;
import javax.servlet.http.*;

/**
 * Stick a SecurityManager in place so certain bad things can't happen.
 */
public class SafetyServlet extends HttpServlet {

	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		try {
			SafetySecurityManager ssm=new SafetySecurityManager();
			System.setSecurityManager(ssm);
			log("Set SecurityManager");
		} catch(SecurityException se) {
			log("Error setting security manager:  " + se);
		}
	}

	public String getServletInfo() {
		return("Copyright (c) 2001  Dustin Sallings <dustin@spy.net>"
			+ " - $Revision: 1.1 $");
	}

}
