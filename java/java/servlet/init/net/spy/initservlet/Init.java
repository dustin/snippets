/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: Init.java,v 1.3 2002/12/04 09:30:43 dustin Exp $
 */

package net.spy.initservlet;

import java.io.*;
import java.net.*;
import java.util.*;

import javax.servlet.*;
import javax.servlet.http.*;

import net.spy.cron.Job;
import net.spy.cron.MainJob;
import net.spy.cron.JobQueue;
import net.spy.cron.Cron;
import net.spy.cron.SimpleTimeIncrement;

import net.spy.pagermusic.RunSubs;

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

		startCron();
	}

	private void startCron() {
		JobQueue jq=new JobQueue();
		cron=new Cron("InitCron", jq);
		jq.addJob(getJob());
		log("Cron is initialized.");
	}

	private Job getJob() {
		Calendar c=Calendar.getInstance();
		c.setTime(new Date());
		c.set(Calendar.HOUR_OF_DAY, 9);
		c.set(Calendar.MINUTE, 0);
		c.set(Calendar.SECOND, 0);
		c.set(Calendar.MILLISECOND, 0);
		Date d=c.getTime();

		String args[]={};
		MainJob j=new MainJob(RunSubs.class.getName(), args,
			d, new SimpleTimeIncrement(86400000));
		return(j);
	}

	/**
	 * Shut down.
	 */
	public void destroy() {
		cron.shutdown();
	}

}
