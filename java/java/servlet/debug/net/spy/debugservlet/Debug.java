/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: Debug.java,v 1.5 2000/10/13 04:55:29 dustin Exp $
 */

package net.spy.debugservlet;

import java.io.*;
import java.util.*;
import java.net.*;
import sun.misc.*;

import javax.servlet.*;
import javax.servlet.http.*;

import net.spy.*;
import net.spy.pool.*;

import java.awt.*;
import java.awt.image.*;

// The class
public class Debug extends HttpServlet
{

	protected String dumpThreadGroup(int depth, ThreadGroup tg) {
		String ret="";
		int n;

		for(int i=0; i<depth; i++) {
			ret+="  ";
		}
		ret+=tg + "\n";

		Thread ts[]=new Thread[tg.activeCount()+10];
		n=tg.enumerate(ts, false);
		for(int i=0; i<n; i++) {
			for(int j=0; j<depth+1; j++) {
				ret+="  ";
			}
			ret+=ts[i] + "\n";
		}
		ret+="\n";

		ThreadGroup tgs[]=new ThreadGroup[tg.activeGroupCount()+10];
		n=tg.enumerate(tgs, false);

		for(int i=0; i<n; i++) {
			ret+=dumpThreadGroup(depth+1, tgs[i]);
		}

		return(ret);
	}

	protected String dumpThreads() {
		Thread t=Thread.currentThread();

		ThreadGroup tg=t.getThreadGroup();

		while(tg.getParent()!=null) { tg=tg.getParent(); }

		return(dumpThreadGroup(0, tg));
	}

	protected String dumpSessions(HttpSession session) {
		String ret="";

		String myid=session.getId();

		HttpSessionContext context=session.getSessionContext();

		for(Enumeration e=context.getIds(); e.hasMoreElements();) {
			String id = (String)e.nextElement();
			HttpSession s=context.getSession(id);

			long now=System.currentTimeMillis();
			long last=s.getLastAccessedTime();
			long diff=(now-last)/1000;

			ret+=id + " Idle for " + diff + "s";

			if(id.equals(myid)) {
				ret+="  <- You are here.";
			}

			ret+="\n";

			String names[]=s.getValueNames();
			for(int i=0; i<names.length; i++) {
				ret+="  " + names[i] + " - ";
				Object o=s.getValue(names[i]);
				Class c=o.getClass();
				ret+=c.getName() + "\n";
			}
		}

		return(ret);
	}

	// Do a GET request
	public void doGet (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {
		String stuff="";
		response.setContentType("text/plain");
		PrintWriter out = response.getWriter();
		ObjectPool op=new ObjectPool(new SpyConfig());
		stuff+="ObjectPool dump:\n" + op;
		out.println(stuff);
		System.getProperties().list(out);

		stuff ="----------- Runtime Info -------------\n";
		Runtime r=Runtime.getRuntime();
		stuff+="Total memory in JVM:  " + r.totalMemory() + "\n";
		stuff+="Free memory in JVM:   " + r.freeMemory() + "\n";
		stuff+="--------------------------------------\n";

		stuff+="-------------- Threads ---------------\n";
		stuff+=dumpThreads();
		stuff+="--------------------------------------\n";

		stuff+="-------------- Session ---------------\n";
		stuff+=dumpSessions(request.getSession(true));
		stuff+="--------------------------------------\n";

		out.println(stuff);

		out.close();
	}
}
