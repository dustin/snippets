/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: Debug.java,v 1.1 2000/07/26 08:49:04 dustin Exp $
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

	// Do a GET request
	public void doGet (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {
		String stuff="";
		PrintWriter out = response.getWriter();
		ObjectPool op=new ObjectPool(new SpyConfig());
		stuff+="ObjectPool dump:\n" + op;
		out.println(stuff);
		System.getProperties().list(out);

		stuff ="-------------- Threads ---------------\n";
		stuff+=dumpThreads();
		stuff+="--------------------------------------\n";
		out.println(stuff);

		out.close();
	}
}
