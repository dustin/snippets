/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: Debug.java,v 1.6 2000/10/13 08:28:36 dustin Exp $
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

	// The servlet context (for dumping servlets)
	ServletContext context=null;

	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		context=config.getServletContext();
	}


	protected String dumpThreadGroup(int depth, ThreadGroup tg, String sc) {
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
			ret+=ts[i];
			// If sc isn't null, show the class name.
			if(sc!=null) {
				ret+=" [" + ts[i].getClass().getName() + "]";
			}
			ret+="\n";
		}
		ret+="\n";

		ThreadGroup tgs[]=new ThreadGroup[tg.activeGroupCount()+10];
		n=tg.enumerate(tgs, false);

		for(int i=0; i<n; i++) {
			ret+=dumpThreadGroup(depth+1, tgs[i], sc);
		}

		return(ret);
	}

	protected String dumpThreads(String showclasses) {
		Thread t=Thread.currentThread();

		ThreadGroup tg=t.getThreadGroup();

		while(tg.getParent()!=null) { tg=tg.getParent(); }

		return(dumpThreadGroup(0, tg, showclasses));
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

	protected String dumpServlets() {
		String ret="";

		for(Enumeration e=context.getServletNames(); e.hasMoreElements(); ) {
			String servletName=(String)e.nextElement();
			Servlet s=null;

			ret+="  " + servletName + "\n";
			try {
				s=context.getServlet(servletName);
				String si=s.getServletInfo();
				if(si==null) {
					si="[No info for this servlet]";
				}
				ret+="\t" + si + "\n";
			} catch(Exception ex) {
				ret+="\t[Error getting servlet info:  " + ex + "]\n";
			}

		}

		return(ret);
	}

	public String getJservSpecific(HttpServletRequest request) {

		String stuff="";

		String jservPrefix="org.apache.jserv.";
		String jservStuff=jservPrefix + "attribute_names";
		if(request.getAttribute(jservStuff)!=null) {
			try {
				stuff+="------- Jserv Specific Stuff ---------\n";
				for(Enumeration e=(Enumeration)request.getAttribute(jservStuff);
					e.hasMoreElements(); ) {

					String attr = e.nextElement().toString();
					stuff+="  " + attr + "=";
					if ( request.getAttribute(jservPrefix + attr) != null ) {
						stuff+= request.getAttribute(jservPrefix
							+ attr.toString()) + "\n";
					} else {
						stuff+="[NULL]\n";
					}
				}
				stuff+="--------------------------------------\n";
			} catch(Exception e) {
			}
		}

		return(stuff);

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

		// Get any jserv specific info we can get.
		stuff+=getJservSpecific(request);

		stuff+="----------- Runtime Info -------------\n";
		Runtime r=Runtime.getRuntime();
		stuff+="Total memory in JVM:  " + r.totalMemory() + "\n";
		stuff+="Free memory in JVM:   " + r.freeMemory() + "\n";
		stuff+="--------------------------------------\n";

		stuff+="-------------- Threads ---------------\n";
		stuff+=dumpThreads(request.getParameter("show_thread_classes"));
		stuff+="--------------------------------------\n";

		stuff+="-------------- Servlets --------------\n";
		stuff+=dumpServlets();
		stuff+="--------------------------------------\n";

		stuff+="-------------- Session ---------------\n";
		stuff+=dumpSessions(request.getSession(true));
		stuff+="--------------------------------------\n";

		out.println(stuff);

		out.close();
	}
}
