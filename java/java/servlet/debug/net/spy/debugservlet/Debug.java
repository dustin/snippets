/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: Debug.java,v 1.7 2000/10/13 09:38:35 dustin Exp $
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


	protected String dumpThreadGroup(ThreadGroup tg, String sc) {
		String ret="";
		int n;

		ret+="<ul>\n";

		ret+="<li>" + tg + "<ul>\n";

		Thread ts[]=new Thread[tg.activeCount()+10];
		n=tg.enumerate(ts, false);
		for(int i=0; i<n; i++) {
			ret+="<li>\n";

			// Show the thread's operational status.
			String status=null;
			try {
				if(ts[i].isAlive()) {
					status="+ ";
				} else {
					status="- ";
				}
			} catch(Exception e) {
				status="? ";
			}
			ret+=status;

			// Add the name.
			ret+=ts[i];
			// If sc isn't null, show the class name.
			if(sc!=null) {
				ret+=" [" + ts[i].getClass().getName() + "]";
			}
			ret+="</li>\n";
		}
		ret+="</ul></ul>\n";

		ThreadGroup tgs[]=new ThreadGroup[tg.activeGroupCount()+10];
		n=tg.enumerate(tgs, false);

		for(int i=0; i<n; i++) {
			ret+=dumpThreadGroup(tgs[i], sc);
		}

		return(ret);
	}

	protected String dumpThreads(String showclasses) {
		Thread t=Thread.currentThread();

		ThreadGroup tg=t.getThreadGroup();

		while(tg.getParent()!=null) { tg=tg.getParent(); }

		return(dumpThreadGroup(tg, showclasses));
	}

	protected String dumpSessions(HttpSession session) {
		String ret="<ul>\n";

		String myid=session.getId();

		HttpSessionContext context=session.getSessionContext();

		for(Enumeration e=context.getIds(); e.hasMoreElements();) {
			String id = (String)e.nextElement();
			HttpSession s=context.getSession(id);

			long now=System.currentTimeMillis();
			long last=s.getLastAccessedTime();
			long diff=(now-last)/1000;

			ret+= "<li><code>" + id + "</code> Idle for " + diff + "s";

			if(id.equals(myid)) {
				ret+="  &lt;- You are here.";
			}

			ret+="\n<ul>\n";

			String names[]=s.getValueNames();
			for(int i=0; i<names.length; i++) {
				ret+="<li>" + names[i] + " - ";
				Object o=s.getValue(names[i]);
				Class c=o.getClass();
				ret+=c.getName() + "</li>\n";
			}
			ret+="</ul></li>\n";
		}

		ret+="</ul>\n";
		return(ret);
	}

	protected String dumpServlets() {
		String ret="<ul>\n";

		for(Enumeration e=context.getServletNames(); e.hasMoreElements(); ) {
			String servletName=(String)e.nextElement();
			Servlet s=null;

			ret+="<li>" + servletName + "\n<ul>\n";
			try {
				s=context.getServlet(servletName);
				String si=s.getServletInfo();
				if(si==null) {
					si="[No info for this servlet]";
				}
				ret+="\t<li>" + si + "</li>\n";
			} catch(Exception ex) {
				ret+="\t[Error getting servlet info:  " + ex + "]\n";
			}
			ret+="</ul></li>\n";

		}

		ret+="</ul>\n";
		return(ret);
	}

	public String getJservSpecific(HttpServletRequest request) {

		String stuff="";

		String jservPrefix="org.apache.jserv.";
		String jservStuff=jservPrefix + "attribute_names";
		if(request.getAttribute(jservStuff)!=null) {
			try {
				stuff+="<h1>Jserv Specific Stuff</h1>\n<ul>\n";
				for(Enumeration e=(Enumeration)request.getAttribute(jservStuff);
					e.hasMoreElements(); ) {

					String attr = e.nextElement().toString();
					stuff+="<li>" + attr + "=";
					if ( request.getAttribute(jservPrefix + attr) != null ) {
						stuff+= request.getAttribute(jservPrefix
							+ attr.toString()) + "\n";
					} else {
						stuff+="[NULL]\n";
					}
				}
				stuff+="</ul>\n";
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
		// Doing HTML
		response.setContentType("text/html");
		PrintWriter out = response.getWriter();
		ObjectPool op=new ObjectPool(new SpyConfig());

		out.println(
			"<html><head><title>DebugServlet</title></head>\n"
			+ "<body bgcolor=\"#FfFfFf\">"
		);

		// Gotta do this with out.print because of the damned properties list
		out.print("<h1>System Properties</h1>\n<pre>\n");
		System.getProperties().list(out);
		out.print("</pre>\n");

		stuff+="<h1>ObjectPool Dump</h1>\n";
		stuff+="<pre>\nObjectPool dump:\n" + op + "\n</pre>\n";

		// Get any jserv specific info we can get.
		stuff+=getJservSpecific(request);

		stuff+="<h1>Runtime Info</h1>\n";
		Runtime r=Runtime.getRuntime();
		stuff+="Total memory in JVM:  " + r.totalMemory() + "<br>\n";
		stuff+="Free memory in JVM:   " + r.freeMemory() + "<br>\n";

		stuff+="<h1>Threads</h1>\n";
		stuff+=dumpThreads(request.getParameter("show_thread_classes"));

		stuff+="<h1>Servlets</h1>\n";
		stuff+=dumpServlets();

		stuff+="<h1>Sessions</h1>\n";
		stuff+=dumpSessions(request.getSession(true));

		out.println(stuff);

		out.println("</body></html>");

		out.close();
	}
}
