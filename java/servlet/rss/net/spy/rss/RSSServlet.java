// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: RSSServlet.java,v 1.7 2003/03/24 19:56:44 dustin Exp $

package net.spy.rss;

import java.io.*;
import java.net.URL;

import javax.servlet.*;
import javax.servlet.http.*;

import com.caucho.transform.*;
import com.caucho.xsl.*;

import net.spy.net.*;

public class RSSServlet extends HttpServlet {

	// process post requests
	public void doPost(
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {
		doGet(request, response);
	}

	// Process get requests.
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
	 * Get the HTML for a specific RSS and stylesheet.
	 */
	public static String getHTML(String url, String stylesheet)
		throws Exception {

		URL u=new URL(url);

		// Grab the URLWatcher
		URLWatcher uw=URLWatcher.getInstance();

		// Get the content from the URLWatcher
		String xml=uw.getContent(u);
		if(xml==null) {
			xml="<no_data_found/>";
		}
		OutputStream out=new ByteArrayOutputStream();
		StylesheetFactory factory=new Xsl();
		Stylesheet style=factory.newStylesheet(stylesheet);
		StreamTransformer transformer=style.newStreamTransformer();
		transformer.transformString(xml, out);

		return(out.toString());
	}

}
