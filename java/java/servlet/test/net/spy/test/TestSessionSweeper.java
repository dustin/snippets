/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: TestSessionSweeper.java,v 1.1 1999/11/15 08:39:28 dustin Exp $
 */

package net.spy.test;

import java.sql.*;
import java.lang.*;
import java.util.*;
import java.io.*;
import sun.misc.*;

import javax.servlet.*;
import javax.servlet.http.*;


import net.spy.*;

public class TestSessionSweeper extends Thread {

	HttpSessionContext context=null;

	// Constructor
	public TestSessionSweeper(HttpSessionContext context) {
		super("session_sweeper");
		this.setDaemon(true);
		this.context=context;
	}

	protected void doSweep() {
		long now=System.currentTimeMillis();
		TestConfig t = new TestConfig();
		for(Enumeration e=context.getIds(); e.hasMoreElements();) {
			String id = (String)e.nextElement();
			HttpSession session=context.getSession(id);
			long idle=now - session.getLastAccessedTime();
			long age=now - session.getCreationTime();
			long max_idle=Long.valueOf(t.get("sweeper_maxage")).longValue();
			System.err.println("Session " + id + " is " + age +
				" millis old and " + idle + " millis idle" );
			if(idle> (1000 * max_idle)) {
				System.err.println("Invalidating " + id);
				session.invalidate();
			}
		}
	}

	public void run() {
		for(;;) {
			try {
				TestConfig t = new TestConfig();
				int m=Integer.valueOf(t.get("sweeper_sleep")).intValue();
				// Check every x minutes
				sleep(m * 1000);
			} catch(Exception e) {
			} finally {
				doSweep();
			}
		}
	}
}
