// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: QuakeServlet.java,v 1.2 2002/08/16 07:18:37 dustin Exp $

package net.spy.quake;

import java.io.*;
import java.util.*;
import net.spy.net.*;

import javax.servlet.*;
import javax.servlet.http.*;

public class QuakeServlet extends HttpServlet {

	// Process get requests only.
	public void doGet(
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {
		try {
			// Get it
			String rss=getRSS();

			// Give it
			response.setContentType("text/plain");
			PrintWriter out = response.getWriter();
			out.print(rss);
			out.close();
		} catch(Exception e) {
			e.printStackTrace();
			throw new ServletException("Error processing quake info:  " + e);
		}
	}

	public static String getRSS() throws Exception {

		String qurl="http://quake.wr.usgs.gov/recenteqs/Maps/SF_Bay.html";
		HTTPFetch hf=new HTTPFetch(qurl);
		String data=hf.getData();
		ArrayList useful=new ArrayList();
		StringBuffer rss=new StringBuffer();

		StringTokenizer st=new StringTokenizer(data, "\r\n");

		while(st.hasMoreTokens()) {
			String tmp=st.nextToken();

			// Yeah, this parser's a little lame, so what?
			if(tmp.indexOf("/recenteqs/Quakes/")>0) {
				StringBuffer title=null;
				StringBuffer url=null;
				String date=null;
				int l=tmp.indexOf("<");
				if(l>0) {
					title=new StringBuffer();
					title.append(tmp.substring(0, l));
					int r=tmp.indexOf(">");
					if(r>0) {
						date=tmp.substring(r+1, r+20);
						r=tmp.indexOf(">", r+1);
						if(r>0) {
							title.append(tmp.substring(r+1));
						} else {
							title=null;
						}
					} else {
						title=null;
					}

				}

				l=tmp.indexOf("\"");
				if(l>0) {
					url=new StringBuffer();
					int r=tmp.indexOf("\"", l+1);
					if(r>0) {
						url.append(tmp.substring(l+1, r));
					} else {
						url=null;
					}
				}

				if(title!=null) {
					StringBuffer piece=new StringBuffer();
					piece.append("\t\t<item>\n");

					if(url!=null) {
						piece.append("\t\t\t<link>");
						piece.append("http://quake.wr.usgs.gov");
						piece.append(url.toString().trim());
						piece.append("</link>\n");
					}

					piece.append("\t\t\t<title>");
					piece.append("[");
					piece.append(date);
					piece.append("] ");
					piece.append(title.toString().trim());
					piece.append("</title>\n");

					piece.append("\t\t</item>\n");

					useful.add(piece.toString());
				}
			}
		}

		rss.append("<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n");
		rss.append("<!DOCTYPE rss PUBLIC ");
		rss.append("\"-//Netscape Communications//DTD RSS 0.91//EN\"\n\t");
		rss.append("\"http://my.netscape.com/publish/formats/rss-0.91.dtd\">");
		rss.append("\n\n<rss version=\"0.91\">\n");

		rss.append("\t<channel>\n");
		rss.append("\t\t<title>SF Bay Earthquakes</title>\n");
		rss.append("\t\t<link>");
		rss.append(qurl);
		rss.append("</link>\n");

		rss.append("\t\t<image>\n");
		rss.append("\t\t\t<title>SF Bay Earthquakes</title>\n");
		rss.append("\t\t\t<link>");
		rss.append(qurl);
		rss.append("</link>\n");
		rss.append("\t\t\t<width>58</width>\n");
		rss.append("\t\t\t<height>35</height>\n");
		rss.append("\t\t\t<url>");
		rss.append("http://quake.wr.usgs.gov/recenteqs/Logos/mini_logo.gif");
		rss.append("</url>\n");
		rss.append("\t\t</image>\n");

		for(Iterator i=useful.iterator(); i.hasNext(); ) {
			String tmp=(String)i.next();

			rss.append(tmp);
		}

		rss.append("\t</channel>\n");
		rss.append("</rss>\n");

		return(rss.toString().trim());
	}

	public static void main(String args[]) throws Exception {
		String html=getRSS();

		System.out.println(html);
	}

}
