/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: XMLRPC.java,v 1.1 2002/03/05 05:40:33 dustin Exp $
 */

package net.spy.rpc;

import java.io.*;
import java.net.*;
import java.util.*;

import javax.servlet.*;
import javax.servlet.http.*;

import org.apache.xmlrpc.*;

import net.spy.*;

/**
 * XML RPC Servlet.
 */
public class XMLRPC extends HttpServlet
{

	private XmlRpcServer xmlrpc=null;
	private SpyConfig conf=null;

	/**
	 * This will initialize the RPC servlet to provide RPC services as
	 * configured in the configuration path.
	 */
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		File f=new File("/afs/spy.net/misc/web/etc/xmlrpc.conf");
		conf=new SpyConfig(f);

		xmlrpc=new XmlRpcServer();

		log("Initializing RPC Services.");
		for(Enumeration e=conf.propertyNames(); e.hasMoreElements();) {
			String key=(String)e.nextElement();
			String value=conf.get(key);

			try {
				Class c=Class.forName(value);
				Object o=c.newInstance();

				log("...adding " + key + " handler");
				xmlrpc.addHandler(key, o);
			} catch(Exception ex) {
				log("Exception initializing " + key, ex);
			}
		}
	}

	/**
	 * Do our XML thing.
	 */
	protected void doPost(
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException, IOException {

		byte[] result=xmlrpc.execute(request.getInputStream());
		response.setContentType("text/xml");
		response.setContentLength(result.length);
		OutputStream os=response.getOutputStream();
		os.write(result);
		os.flush();
		os.close();
	}
}
