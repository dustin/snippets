// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: RSSServlet.java,v 1.3 2001/04/26 19:16:01 dustin Exp $

package net.spy.rss;

import java.io.*;

import javax.servlet.*;
import javax.servlet.http.*;

import com.caucho.transform.*;
import com.caucho.xsl.*;

public class RSSServlet extends HttpServlet {

	private RSSStore rssstore=null;

	/**
	 * Initialize this thingy.
	 */
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		log("Initializing RSSStore");
		rssstore=new RSSStore();
		rssstore.start();
	}

	/**
	 * What to do when the man shuts us down.
	 */
	public void destory() {
		log("Shutting down RSSStore");
		rssstore.shutdown();
	}

	// Process get requests only.
	public void doGet(
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {
		try {
			// Get it
			String url=request.getParameter("url");
			String xsl=request.getParameter("xsl");
			log("url=" + url + " - xsl=" + xsl);
			String html=getHTML(url, xsl);

			// Set a cache time
			java.util.Date d=new java.util.Date();
			long l=d.getTime();
			// Make it valid for fifteen minutes.
			l+=900000L;
			response.setDateHeader("Expires", l);

			// Give it
			response.setContentType("text/html");
			PrintWriter out = response.getWriter();
			out.print(html);
			out.close();
		} catch(Exception e) {
			e.printStackTrace();
			throw new ServletException("Error processing RSS:  " + e);
		}
	}

	protected String getHTML(String url, String stylesheet)
		throws Exception {

		// Get the content from the RSS store.
		String xml=rssstore.getContent(url);
		OutputStream out=new ByteArrayOutputStream();
		StylesheetFactory factory=new Xsl();
		Stylesheet style=factory.newStylesheet(stylesheet);
		StreamTransformer transformer=style.newStreamTransformer();
		transformer.transformString(xml, out);

		return(out.toString());
	}

}
