// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: RSSServlet.java,v 1.5 2001/09/21 07:48:05 dustin Exp $

package net.spy.rss;

import java.io.*;

import javax.servlet.*;
import javax.servlet.http.*;

import com.caucho.transform.*;
import com.caucho.xsl.*;

public class RSSServlet extends HttpServlet {

	// This is static because pages may directly request stuff from it as
	// long as it's initialized and properly in a servlet engine.
	private static RSSStore rssstore=null;

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
	public void destroy() {
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

			sendOutput(response, "text/html", html);
		} catch(Exception e) {
			e.printStackTrace();
			sendOutput(response, "text/html", "<!-- " + e + "-->");
		}
	}

	// Send the response
	private void sendOutput(HttpServletResponse response,
		String type, String data) throws IOException {
		response.setContentType(type);
		PrintWriter out = response.getWriter();
		out.print(data);
		out.close();
	}

	/**
	 * Get the HTML for a specific RSS and stylesheet.  This only works in
	 * a servlet environment when the RSSServlet has been properly
	 * initialized.
	 */
	public static String getHTML(String url, String stylesheet)
		throws Exception {

		// This only works under the proper conditions.
		if(rssstore==null) {
			throw new Exception("RSSStore has not been initialized.");
		}

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
