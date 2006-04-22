/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: Init.java,v 1.4 2002/12/08 08:39:40 dustin Exp $
 */

package net.spy.init;

import java.util.Locale;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import net.spy.SpyObject;

/**
 * Perform global servlet engine initialization.
 */
public class Init extends SpyObject implements ServletContextListener {

	/**
	 * Just perform basic initialization.
	 */
	public void contextInitialized(ServletContextEvent contextEvent) {
		Locale.setDefault(Locale.US);
		Locale l=Locale.getDefault();
		getLogger().info("Default locale is now %s", l);
	}

	/**
	 * Shut down.
	 */
	public void contextDestroyed(ServletContextEvent contextEvent) {
		getLogger().info("Shutting down init context.");
	}

}
