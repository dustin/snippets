/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: Init.java,v 1.4 2002/12/08 08:39:40 dustin Exp $
 */

package net.spy.initservlet;

import java.io.*;
import java.net.*;
import java.util.*;

import javax.servlet.*;
import javax.servlet.http.*;

import net.spy.cron.Cron;
import net.spy.cron.FileJobQueue;

/**
 * Perform global servlet engine initialization.
 */
public class Init extends HttpServlet
{
	private Cron cron=null;

	/**
	 * Just perform basic initialization.
	 */
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		Locale.setDefault(Locale.US);
		Locale l=Locale.getDefault();

		log("Default locale is now " + l);

		startCron(config);
	}

	private File getConfigPath(ServletConfig config, String varName)
		throws ServletException {

		String c=config.getInitParameter(varName);
		if(c == null) {
			throw new ServletException("Misconfiguration!  Parameter "
				+ varName + " not included!");
		}

		// If it starts with /WEB-INF, map it to the real location
		if(c.startsWith("/WEB-INF")) {
			c=config.getServletContext().getRealPath(c);
		}

		File rv=new File(c);

		return(rv);
	}

	private void startCron(ServletConfig config) throws ServletException {
		File crontab=getConfigPath(config, "crontab");

		try {
			log("Starting cron services from " + crontab);
			FileJobQueue jq=new FileJobQueue(crontab);
			cron=new Cron(jq);
			log("Cron is ready and running");
		} catch(IOException e) {
			throw new ServletException("Problem initializing cron", e);
		}
	}

	/**
	 * Shut down.
	 */
	public void destroy() {
		log("Shutting down cron.");
		cron.shutdown();
	}

}
