// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: RSSServlet.java,v 1.2 2001/03/02 02:40:02 dustin Exp $

package net.spy.rss;

import java.io.*;
import net.spy.net.*;

import javax.servlet.*;
import javax.servlet.http.*;

import com.caucho.transform.*;
import com.caucho.xsl.*;

public class RSSServlet extends HttpServlet {

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
			// Make it valid for an hour.
			l+=3600000L;
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

	protected static String getHTML(String url, String stylesheet)
		throws Exception {

		HTTPFetch hf=new HTTPFetch(url);
		String xml=hf.getData();
		OutputStream out=new ByteArrayOutputStream();
		StylesheetFactory factory=new Xsl();
		Stylesheet style=factory.newStylesheet(stylesheet);
		StreamTransformer transformer=style.newStreamTransformer();
		transformer.transformString(xml, out);

		return(out.toString());
	}

	public static void main(String args[]) throws Exception {
		String html=getHTML(args[0], args[1]);

		System.out.println(html);
	}

}
